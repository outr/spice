package spec

import fabric.*
import fabric.dsl.*
import fabric.io.JsonFormatter
import fabric.rw.*
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.{AnyWordSpec, AsyncWordSpec}
import spice.http.HttpMethod
import spice.net.{URLPath, *}
import spice.openapi.{OpenAPIContent, OpenAPIContentType, OpenAPIResponse, OpenAPISchema}
import spice.openapi.server.{OpenAPIHttpServer, Schema, Service, ServiceCall}

class OpenAPIServerAdvancedSpec extends AnyWordSpec with Matchers {
  "OpenAPIServer Advanced" should {
    "generate OpenAPI spec with path parameters" in {
      val json = AdvancedOpenAPIServer.api.asJson
      // Verify path parameters are generated for /board/{row}/{column}
      val boardSquarePath = json("paths")("/board/{row}/{column}")
      val params = boardSquarePath("parameters")
      params.isArr should be(true)
      params.asVector.length should be(2)
      params.asVector.head("$ref").asString should be("#/components/parameters/rowParam")
      params.asVector(1)("$ref").asString should be("#/components/parameters/columnParam")

      // Verify component parameters are generated
      val components = json("components")("parameters")
      components("rowParam")("name").asString should be("row")
      components("rowParam")("in").asString should be("path")
      components("rowParam")("required").asBoolean should be(true)
      components("rowParam")("schema")("type").asString should be("integer")
      components("columnParam")("name").asString should be("column")
      components("columnParam")("in").asString should be("path")
      components("columnParam")("schema")("type").asString should be("integer")

      // Verify /board has no parameters
      val boardPath = json("paths")("/board")
      boardPath.get("parameters").forall(p => p.isArr && p.asVector.isEmpty) should be(true)
    }

    "generate OpenAPI spec with error responses" in {
      val json = AdvancedOpenAPIServer.api.asJson
      // Verify error response on boardSquare GET
      val getResponses = json("paths")("/board/{row}/{column}")("get")("responses")
      getResponses("200")("description").asString should be("OK")
      getResponses("400")("description").asString should be("The provided parameters are incorrect")
    }
  }

  object AdvancedOpenAPIServer extends OpenAPIHttpServer { s =>
    override def title: String = "Tic Tac Toe"
    override def version: String = "1.0.0"
    override def description: Option[String] = Some("This API allows writing down marks on a Tic Tac Toe board and requesting the state of the board or of individual squares.")
    override def tags: List[String] = List("gameplay")

    object board extends Service {
      override def server: OpenAPIHttpServer = s

      override val path: URLPath = path"/board"

      override val calls: List[ServiceCall] = List(
        serviceCall[Unit, Status](
          method = HttpMethod.Get,
          summary = "Get the whole board",
          description = "Retrieves the current state of the board and the winner.",
          successDescription = "OK",
          tags = List("Gameplay"),
          operationId = Some("get-board"),
          responseSchema = Some(Schema(
            properties = Map(
              "winner" -> Schema(
                description = Some("Winner of the game. `.` means nobody has won yet."),
                example = Some(".")
              ),
              "board" -> Schema(
                maxItems = Some(3),
                minItems = Some(3),
                items = Some(Schema(
                  maxItems = Some(3),
                  minItems = Some(3),
                  items = Some(Schema(
                    description = Some("Possible values for a board square. `.` means empty square."),
                    example = Some(".")
                  ))
                ))
              )
            )
          ))
        ) { request =>
          request.response(Status(
            winner = Winner.`.`,
            board = List(List())
          ))
        }
      )
    }

    object boardSquare extends Service {
      override def server: OpenAPIHttpServer = s

      override val path: URLPath = path"/board/{row}/{column}"

      override val calls: List[ServiceCall] = List(
        serviceCall[Square, Mark](
          method = HttpMethod.Get,
          summary = "Get a single board square",
          description = "Retrieves the requested square.",
          tags = List("Gameplay"),
          operationId = Some("get-square"),
          successDescription = "OK",
          responseSchema = Some(Schema(
            description = Some("Possible values for a board square. `.` means empty square."),
            example = Some(".")
          )),
          errorResponses = Map(
            "400" -> OpenAPIResponse(
              description = "The provided parameters are incorrect",
              content = OpenAPIContent(
                ContentType.`text/html` -> OpenAPIContentType(
                  schema = OpenAPISchema.Component(`type` = "string", maxLength = Some(256))
                )
              )
            )
          )
        ) { request =>
          request.response(Mark.`.`)
        }
      )
    }

    override def services: List[Service] = List(
      board, boardSquare
    )

    case class Status(winner: Winner, board: List[List[Mark]])

    object Status {
      given rw: RW[Status] = RW.gen
    }

    case class Winner(value: String)

    object Winner {
      given rw: RW[Winner] = RW.enumeration(
        list = List(`.`, X, O),
        asString = (w: Winner) => w.value,
        caseSensitive = false
      )

      lazy val `.`: Winner = Winner(".")
      lazy val X: Winner = Winner("X")
      lazy val O: Winner = Winner("O")
    }

    case class Mark(value: String)

    object Mark {
      given rw: RW[Mark] = RW.enumeration(
        list = List(`.`, X, O),
        asString = (m: Mark) => m.value,
        caseSensitive = false
      )

      lazy val `.`: Mark = Mark(".")
      lazy val X: Mark = Mark("X")
      lazy val O: Mark = Mark("O")
    }

    case class Square(row: Int, column: Int)

    object Square {
      given rw: RW[Square] = RW.gen
    }
  }
}