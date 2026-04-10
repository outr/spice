package spice.mcp

import fabric.*
import fabric.io.{JsonFormatter, JsonParser}
import fabric.rw.*
import rapid.Task
import scribe.mdc.MDC
import spice.http.*
import spice.http.content.{Content, JsonContent, StringContent}
import spice.http.server.handler.HttpHandler

import java.util.UUID
import java.util.concurrent.ConcurrentHashMap

case class MCPHandler(server: MCPServer) extends HttpHandler {

  private val sessions = new ConcurrentHashMap[String, MCPSession]()

  private val mcpPath: String = server.mcpPath

  override def handle(exchange: HttpExchange)(using mdc: MDC): Task[HttpExchange] = {
    val path = exchange.request.url.path.decoded
    if (path != mcpPath) {
      Task.pure(exchange)
    } else {
      exchange.request.method match {
        case HttpMethod.Post => handlePost(exchange)
        case HttpMethod.Delete => handleDelete(exchange)
        case _ => Task.pure(exchange)
      }
    }
  }

  private def handlePost(exchange: HttpExchange)(using MDC): Task[HttpExchange] = {
    val acceptHeader = exchange.request.headers.first(Headers.Request.`Accept`).getOrElse("")
    if (!acceptHeader.contains("application/json") && !acceptHeader.contains("*/*")) {
      respondError(exchange, Null, JsonRPCError(-32600, "Accept header must include application/json"))
    } else {
      parseRequestBody(exchange).flatMap {
        case None =>
          respondError(exchange, Null, JsonRPCError.ParseError)
        case Some(request) =>
          val id = request.id.getOrElse(Null)
          request.method match {
            case "initialize" => handleInitialize(exchange, id, request.params)
            case "notifications/initialized" => respondSuccess(exchange, id, obj())
            case "ping" => handleWithSession(exchange, id) { _ => respondSuccess(exchange, id, obj()) }
            case "tools/list" => handleWithSession(exchange, id) { _ => handleToolsList(exchange, id) }
            case "tools/call" => handleWithSession(exchange, id) { ctx => handleToolsCall(exchange, id, request.params, ctx) }
            case "resources/list" => handleWithSession(exchange, id) { _ => handleResourcesList(exchange, id) }
            case "resources/read" => handleWithSession(exchange, id) { ctx => handleResourcesRead(exchange, id, request.params, ctx) }
            case _ => respondError(exchange, id, JsonRPCError.MethodNotFound)
          }
      }
    }
  }

  private def handleWithSession(exchange: HttpExchange, id: Json)(f: MCPContext => Task[HttpExchange])(using MDC): Task[HttpExchange] = {
    val sessionId = exchange.request.headers.first(new StringHeaderKey("Mcp-Session-Id")).getOrElse("")
    val session = Option(sessions.get(sessionId))
    session match {
      case Some(s) =>
        authenticateRequest(exchange).flatMap {
          case Some(ctx) => f(ctx.copy(sessionId = sessionId))
          case None => respondUnauthorized(exchange)
        }
      case None =>
        respondError(exchange, id, JsonRPCError(-32600, "Invalid or missing Mcp-Session-Id"))
    }
  }

  private def authenticateRequest(exchange: HttpExchange)(using MDC): Task[Option[MCPContext]] = {
    server.authenticateMCP(exchange).flatMap {
      case some @ Some(_) => Task.pure(some)
      case None =>
        val bearerToken = exchange.request.headers.first(Headers.Request.`Authorization`)
          .filter(_.startsWith("Bearer "))
          .map(_.substring(7))
        bearerToken match {
          case Some(token) =>
            server.oauthStore.getToken(token).map(_.map { accessToken =>
              MCPContext(sessionId = "", store = accessToken.context)
            })
          case None => Task.pure(None)
        }
    }
  }

  private def handleInitialize(exchange: HttpExchange, id: Json, params: Option[Json])(using MDC): Task[HttpExchange] = {
    authenticateRequest(exchange).flatMap {
      case None => respondUnauthorized(exchange)
      case Some(_) =>
        val sessionId = UUID.randomUUID().toString
        sessions.put(sessionId, MCPSession(sessionId))

        val capabilities = obj(
          "tools" -> (if (server.tools.nonEmpty) obj("listChanged" -> false.json) else obj()),
          "resources" -> (if (server.resources.nonEmpty) obj("listChanged" -> false.json) else obj())
        )

        val result = obj(
          "protocolVersion" -> str("2025-03-26"),
          "capabilities" -> capabilities,
          "serverInfo" -> obj(
            "name" -> str(server.mcpName),
            "version" -> str(server.mcpVersion)
          )
        )

        respondSuccess(exchange, id, result).map { e =>
          e.copy(response = e.response.withHeader("Mcp-Session-Id", sessionId))
        }
    }
  }

  private def handleToolsList(exchange: HttpExchange, id: Json)(using MDC): Task[HttpExchange] = {
    val toolDefs = server.tools.map { tool =>
      obj(
        "name" -> str(tool.definition.name),
        "description" -> str(tool.definition.description),
        "inputSchema" -> tool.definition.inputSchema
      )
    }
    respondSuccess(exchange, id, obj("tools" -> arr(toolDefs*)))
  }

  private def handleToolsCall(exchange: HttpExchange, id: Json, params: Option[Json], ctx: MCPContext)(using MDC): Task[HttpExchange] = {
    val toolName = params.flatMap(_.get("name")).map(_.asString).getOrElse("")
    val arguments = params.flatMap(_.get("arguments")).getOrElse(obj())
    server.tools.find(_.definition.name == toolName) match {
      case None =>
        respondError(exchange, id, JsonRPCError(-32602, s"Unknown tool: $toolName"))
      case Some(tool) =>
        given MCPContext = ctx
        tool.call(arguments).flatMap { result =>
          val contentJson = result.content.map(_.toJson)
          val resultJson = obj(
            "content" -> arr(contentJson*),
            "isError" -> result.isError.json
          )
          respondSuccess(exchange, id, resultJson)
        }.handleError { throwable =>
          scribe.error(s"Tool call failed: $toolName", throwable)
          val errorResult = obj(
            "content" -> arr(obj("type" -> str("text"), "text" -> str(throwable.getMessage))),
            "isError" -> true.json
          )
          respondSuccess(exchange, id, errorResult)
        }
    }
  }

  private def handleResourcesList(exchange: HttpExchange, id: Json)(using MDC): Task[HttpExchange] = {
    val resourceDefs = server.resources.map { resource =>
      val d = resource.definition
      val base = obj(
        "uri" -> str(d.uri),
        "name" -> str(d.name),
        "description" -> str(d.description)
      )
      d.mimeType.fold(base)(mt => base.merge(obj("mimeType" -> str(mt))))
    }
    respondSuccess(exchange, id, obj("resources" -> arr(resourceDefs*)))
  }

  private def handleResourcesRead(exchange: HttpExchange, id: Json, params: Option[Json], ctx: MCPContext)(using MDC): Task[HttpExchange] = {
    val uri = params.flatMap(_.get("uri")).map(_.asString).getOrElse("")
    server.resources.find(_.definition.uri == uri) match {
      case None =>
        respondError(exchange, id, JsonRPCError(-32602, s"Unknown resource: $uri"))
      case Some(resource) =>
        given MCPContext = ctx
        resource.read(uri).flatMap { content =>
          val contentObj = obj(
            "uri" -> str(content.uri),
            "text" -> content.text.map(str).getOrElse(Null)
          )
          val withMime = content.mimeType.fold(contentObj)(mt => contentObj.merge(obj("mimeType" -> str(mt))))
          respondSuccess(exchange, id, obj("contents" -> arr(withMime)))
        }
    }
  }

  private def handleDelete(exchange: HttpExchange)(using MDC): Task[HttpExchange] = {
    val sessionId = exchange.request.headers.first(new StringHeaderKey("Mcp-Session-Id")).getOrElse("")
    sessions.remove(sessionId)
    exchange.modify { response =>
      Task.pure(response.withStatus(HttpStatus.NoContent))
    }.map(_.finish())
  }

  private def parseRequestBody(exchange: HttpExchange): Task[Option[JsonRPCRequest]] = {
    exchange.request.content match {
      case Some(content) =>
        content.asString.map { body =>
          try {
            val json = JsonParser(body)
            Some(json.as[JsonRPCRequest])
          } catch {
            case _: Exception => None
          }
        }
      case None => Task.pure(None)
    }
  }

  private def respondSuccess(exchange: HttpExchange, id: Json, result: Json)(using MDC): Task[HttpExchange] = {
    val response = JsonRPCResponse.success(id, result)
    exchange.withContent(Content.json(response.json)).map(_.finish())
  }

  private def respondError(exchange: HttpExchange, id: Json, error: JsonRPCError)(using MDC): Task[HttpExchange] = {
    val response = JsonRPCResponse.error(id, error)
    exchange.withContent(Content.json(response.json)).map(_.finish())
  }

  private def respondUnauthorized(exchange: HttpExchange)(using MDC): Task[HttpExchange] = {
    exchange.modify { response =>
      Task.pure(response
        .withStatus(HttpStatus.Unauthorized)
        .withContent(Content.string("{\"error\":\"Unauthorized\"}", spice.net.ContentType.`application/json`)))
    }.map(_.finish())
  }
}

case class MCPSession(id: String, created: Long = System.currentTimeMillis())
