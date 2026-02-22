# spice
[![CI](https://github.com/outr/spice/actions/workflows/ci.yml/badge.svg)](https://github.com/outr/spice/actions/workflows/ci.yml)

A Scala 3 framework for server and client HTTP communication with a core focus on OpenAPI / Swagger support. Spice provides type-safe HTTP abstractions, a composable server DSL, production middleware, and automatic OpenAPI spec generation.

## Getting Started

Add the dependencies you need to your `build.sbt`:

```scala
// Core HTTP types and utilities
libraryDependencies += "com.outr" %% "spice-core" % "1.0.0"

// HTTP client (pick one implementation)
libraryDependencies += "com.outr" %% "spice-client-jvm" % "1.0.0"    // java.net.http
libraryDependencies += "com.outr" %% "spice-client-okhttp" % "1.0.0" // OkHttp3
libraryDependencies += "com.outr" %% "spice-client-netty" % "1.0.0"  // Netty

// HTTP server (Undertow backend)
libraryDependencies += "com.outr" %% "spice-server-undertow" % "1.0.0"

// OpenAPI spec generation
libraryDependencies += "com.outr" %% "spice-openapi" % "1.0.0"
```

Spice uses the `rapid` library for async operations via `Task`, and `fabric` for JSON serialization.

## URL Parsing

Spice provides compile-time validated URL literals and a full URL parser:

```scala
import spice.net._

// Compile-time validated URL literal
val google = url"https://www.google.com"
// google: URL = URL(
//   protocol = Protocol(
//     scheme = "https",
//     description = "Hypertext Transfer Protocol Secure",
//     rfc = "RFC2818",
//     defaultPort = Some(443)
//   ),
//   host = "www.google.com",
//   port = 443,
//   path = URLPath(List()),
//   parameters = Parameters(List()),
//   fragment = None
// )

// Runtime URL parsing
val parsed = URL.parse("https://api.example.com/v1/users?page=1&limit=10")
// parsed: URL = URL(
//   protocol = Protocol(
//     scheme = "https",
//     description = "Hypertext Transfer Protocol Secure",
//     rfc = "RFC2818",
//     defaultPort = Some(443)
//   ),
//   host = "api.example.com",
//   port = 443,
//   path = URLPath(List(/, Literal("v1"), /, Literal("users"))),
//   parameters = Parameters(
//     List(("page", Param(List("1"))), ("limit", Param(List("10"))))
//   ),
//   fragment = None
// )
parsed.host
// res0: String = "api.example.com"
parsed.path.encoded
// res1: String = "/v1/users"
parsed.parameters.value("page")
// res2: Option[String] = Some("1")

// URL manipulation
val withParams = URL.parse("https://api.example.com/search")
  .withParam("q", "scala http")
  .withParam("lang", "en")
// withParams: URL = URL(
//   protocol = Protocol(
//     scheme = "https",
//     description = "Hypertext Transfer Protocol Secure",
//     rfc = "RFC2818",
//     defaultPort = Some(443)
//   ),
//   host = "api.example.com",
//   port = 443,
//   path = URLPath(List(/, Literal("search"))),
//   parameters = Parameters(
//     List(("lang", Param(List("en"))), ("q", Param(List("scala http"))))
//   ),
//   fragment = None
// )
withParams.toString
// res3: String = "https://api.example.com/search?lang=en&q=scala%20http"

// Multi-part TLD support
val ukUrl = URL.parse("https://www.example.co.uk/path")
// ukUrl: URL = URL(
//   protocol = Protocol(
//     scheme = "https",
//     description = "Hypertext Transfer Protocol Secure",
//     rfc = "RFC2818",
//     defaultPort = Some(443)
//   ),
//   host = "www.example.co.uk",
//   port = 443,
//   path = URLPath(List(/, Literal("path"))),
//   parameters = Parameters(List()),
//   fragment = None
// )
ukUrl.tld
// res4: Option[String] = Some("co.uk")
ukUrl.domain
// res5: String = "example.co.uk"
```

## HTTP Server

### Basic Server with DSL Routing

Spice servers use a composable filter DSL. Filters are chained with `/` and requests flow through them in order:

```scala
import rapid._
import spice.http.server._
import spice.http.server.dsl._
import spice.http.{HttpExchange, HttpMethod}
import spice.http.content.Content
import spice.net._

object MyServer extends MutableHttpServer {
  // Simple route: GET /hello -> "Hello, World!"
  handler(
    HttpMethod.Get / "hello" / Content.string("Hello, World!", ContentType.`text/plain`)
  )
}
```

### Static Server with Multiple Routes

For more complex routing, use `StaticHttpServer` with the `filters` function:

```scala
import rapid._
import spice.http.server._
import spice.http.server.dsl._
import spice.http.server.handler.HttpHandler
import spice.http.{HttpExchange, HttpMethod}
import spice.http.content.Content
import spice.net._

object ApiServer extends StaticHttpServer {
  override protected val handler: HttpHandler = filters(
    // GET /api/health -> health check
    HttpMethod.Get / "api" / "health" / Content.string("""{"status":"ok"}""", ContentType.`application/json`),

    // IP-restricted admin routes
    allow(ip"127.0.0.1") / HttpMethod.Get / "admin" / List(
      "dashboard" / Content.string("<h1>Dashboard</h1>", ContentType.`text/html`),
      "settings" / Content.string("<h1>Settings</h1>", ContentType.`text/html`)
    )
  )
}
```

### Starting a Server

```scala
import rapid._
import spice.http.server._
import spice.http.server.dsl._
import spice.http.server.config._
import spice.http.server.handler.HttpHandler
import spice.http.content.Content
import spice.http.HttpMethod
import spice.net._

object WebServer extends StaticHttpServer {
  // Configure listeners
  config.clearListeners().addListeners(
    HttpServerListener(host = "0.0.0.0", port = Some(8080))
  )

  override protected val handler: HttpHandler = filters(
    HttpMethod.Get / "hello" / Content.string("Hello!", ContentType.`text/plain`)
  )
}

// Start the server
WebServer.start().sync()
// Block until the server stops
WebServer.whileRunning().sync()
```

### CORS Support

Mix in `CORSSupport` for full CORS handling with automatic preflight responses:

```scala
import spice.http.server._
import spice.http.server.dsl._
import spice.http.server.handler.HttpHandler
import spice.http.content.Content
import spice.http.HttpMethod
import spice.net._

object CorsServer extends StaticHttpServer with CORSSupport {
  override protected def allowOrigin: String = "https://myapp.com"
  override protected def allowCredentials: Boolean = true
  override protected def allowHeaders: Set[String] =
    Set("Content-Type", "Authorization", "Accept")

  override protected val handler: HttpHandler = filters(
    HttpMethod.Get / "api" / "data" / Content.string("{}", ContentType.`application/json`)
  )
}
```

## HTTP Client

The HTTP client uses an immutable builder pattern:

```scala
import rapid._
import fabric.rw._
import spice.http.client.HttpClient
import spice.net._

// Simple GET request
val response = HttpClient
  .url(url"https://httpbin.org/get")
  .get
  .send()
  .sync()

println(s"Status: ${response.status}")

// POST with JSON body and type-safe response parsing
case class Todo(userId: Int, id: Int, title: String, completed: Boolean)
object Todo {
  implicit val rw: RW[Todo] = RW.gen
}

val todo = HttpClient
  .url(url"https://jsonplaceholder.typicode.com/todos/1")
  .get
  .call[Todo]
  .sync()

println(s"Todo: ${todo.title}")
```

### Restful Client Calls

For typed request/response patterns:

```scala
import rapid._
import fabric.rw._
import spice.http.client.HttpClient
import spice.net._

case class CreatePost(title: String, body: String, userId: Int)
object CreatePost {
  implicit val rw: RW[CreatePost] = RW.gen
}

case class PostResponse(id: Int, title: String, body: String, userId: Int)
object PostResponse {
  implicit val rw: RW[PostResponse] = RW.gen
}

val result = HttpClient
  .url(url"https://jsonplaceholder.typicode.com/posts")
  .restful[CreatePost, PostResponse](
    CreatePost("My Post", "Post content", 1)
  )
  .sync()
```

### Client Configuration

```scala
import scala.concurrent.duration._
import spice.http.client._
import spice.http.client.intercept.Interceptor
import spice.net._

val client = HttpClient
  .url(url"https://api.example.com")
  .timeout(30.seconds)
  .retryManager(RetryManager.simple(retries = 3, delay = 1.second))
  .interceptor(Interceptor.rateLimited(1.second))
  .failOnHttpStatus(true)
```

## Middleware

Spice provides production-ready middleware as composable `ConnectionFilter` instances.

### Authentication

```scala
import rapid._
import spice.http.server.middleware._

// Basic Auth
val basicAuth = AuthenticationFilter(
  BasicAuthenticator("MyApp") { (username, password) =>
    Task.pure(username == "admin" && password == "secret")
  },
  realm = "MyApp",
  scheme = "Basic"
)

// Bearer Token Auth
val bearerAuth = AuthenticationFilter(
  BearerAuthenticator { token =>
    // Return Some(principal) if valid, None if not
    Task.pure(if (token == "valid-token") Some("user123") else None)
  }
)

// Use in routes:
// bearerAuth / HttpMethod.Get / "api" / "protected" / handler
```

### Security Headers

```scala
import spice.http.server.middleware._

// Use sensible defaults (HSTS, X-Frame-Options: DENY, X-Content-Type-Options: nosniff)
val securityHeaders = SecurityHeadersFilter.Default

// Or customize
val customSecurity = SecurityHeadersFilter(
  hstsMaxAge = Some(31536000L),
  frameOptions = Some("SAMEORIGIN"),
  contentSecurityPolicy = Some("default-src 'self'")
)
```

### Rate Limiting

```scala
import spice.http.server.middleware._

// 100 requests per minute per IP
val rateLimiter = RateLimitFilter(
  maxRequests = 100,
  windowMillis = 60000L
)

// Custom key extractor (e.g., by API key)
val apiKeyLimiter = RateLimitFilter(
  maxRequests = 1000,
  windowMillis = 3600000L,
  keyExtractor = exchange =>
    exchange.request.headers.first(spice.http.Headers.Request.`Authorization`)
      .getOrElse("anonymous")
)
```

### Request Size Limits

```scala
import spice.http.server.middleware._

// Reject requests larger than 10MB
val sizeLimit = MaxContentLengthFilter(10L * 1024L * 1024L)
```

### ETag / Conditional Requests

```scala
import spice.http.server.middleware._

// Automatically generates ETags and handles If-None-Match for 304 responses
val etag = ETagFilter()
```

### Composing Middleware

Middleware composes with the `/` operator, same as routing filters:

```scala
import spice.http.server._
import spice.http.server.dsl._
import spice.http.server.handler.HttpHandler
import spice.http.server.middleware._
import spice.http.content.Content
import spice.http.HttpMethod
import spice.net._

object SecureServer extends StaticHttpServer with CORSSupport {
  val auth = AuthenticationFilter(
    BearerAuthenticator(token =>
      rapid.Task.pure(if (token.nonEmpty) Some(token) else None)
    )
  )

  override protected val handler: HttpHandler = filters(
    SecurityHeadersFilter.Default,
    RateLimitFilter(maxRequests = 100, windowMillis = 60000L),

    // Public routes
    HttpMethod.Get / "health" / Content.string("ok", ContentType.`text/plain`),

    // Protected routes
    auth / HttpMethod.Get / "api" / "profile" / ActionFilter { exchange =>
      val principal = AuthenticationFilter.principal(exchange)
      exchange.modify { response =>
        rapid.Task.pure(response.withContent(
          Content.string(s"""{"user":"${principal.getOrElse("unknown")}"}""", ContentType.`application/json`)
        ))
      }
    }
  )
}
```

### Metrics Hooks

Implement the `MetricsFilter` trait to integrate with your metrics system:

```scala
import rapid.Task
import spice.http.{HttpExchange, HttpStatus}
import spice.http.server.middleware.MetricsFilter

object PrometheusMetrics extends MetricsFilter {
  override def onRequestStart(exchange: HttpExchange): Task[Unit] =
    Task(println(s"Request started: ${exchange.request.url.path}"))

  override def onRequestComplete(exchange: HttpExchange, durationMs: Long, status: HttpStatus): Task[Unit] =
    Task(println(s"Request completed: $status in ${durationMs}ms"))

  override def onRequestError(exchange: HttpExchange, throwable: Throwable): Task[Unit] =
    Task(println(s"Request error: ${throwable.getMessage}"))
}
```

## OpenAPI

Spice generates OpenAPI 3.0.3 specs from type-safe service definitions and automatically serves them at `/openapi.json` and `/openapi.yaml`.

### Defining Services

```scala
import rapid._
import fabric.rw._
import spice.http.HttpMethod
import spice.http.server.config.HttpsServerListener
import spice.net._
import spice.openapi.server._

// Define your request/response types with fabric RW derivation
case class User(name: String, email: String)
object User {
  implicit val rw: RW[User] = RW.gen
}

case class CreateUserRequest(name: String, email: String, password: String)
object CreateUserRequest {
  implicit val rw: RW[CreateUserRequest] = RW.gen
}

// Define the OpenAPI server
object ApiServer extends OpenAPIHttpServer {
  override def title: String = "User API"
  override def version: String = "1.0.0"
  override def description: Option[String] = Some("User management API")

  config.clearListeners().addListeners(
    HttpsServerListener(host = "api.example.com", basePath = path"/v1")
  )

  // Define services using the Service trait
  object usersService extends Service {
    override def server: OpenAPIHttpServer = ApiServer
    override val path: URLPath = path"/users"
    override val calls: List[ServiceCall] = List(
      serviceCall[Unit, List[User]](
        method = HttpMethod.Get,
        summary = "List all users",
        description = "Returns a list of all registered users",
        successDescription = "A JSON array of users"
      ) { request =>
        request.response(List(
          User("Alice", "alice@example.com"),
          User("Bob", "bob@example.com")
        ))
      }
    )
  }

  override lazy val services: List[Service] = List(usersService)
}
```

### Using RestService Helper

For simpler typed services, use `RestService`:

```scala
import rapid._
import fabric.rw._
import spice.net._
import spice.openapi.server._

case class ReverseRequest(text: String)
object ReverseRequest {
  implicit val rw: RW[ReverseRequest] = RW.gen
}

case class ReverseResponse(result: String)
object ReverseResponse {
  implicit val rw: RW[ReverseResponse] = RW.gen
}

object MyAPI extends OpenAPIHttpServer {
  override def title: String = "Text API"
  override def version: String = "1.0.0"

  private val reverseService = RestService[ReverseRequest, ReverseResponse](
    this, path"/reverse", "Reverse text"
  ) { request =>
    Task.pure(ReverseResponse(request.text.reverse))
  }

  override lazy val services: List[Service] = List(reverseService)
}

// The spec is available programmatically
// MyAPI.api.asJson  -> JSON OpenAPI spec
// MyAPI.api.asYaml  -> YAML OpenAPI spec
```

## WebSockets

### Server-Side WebSocket Handler

```scala
import rapid._
import spice.http.{HttpExchange, WebSocketListener}
import spice.http.server._
import spice.http.server.dsl._
import spice.http.server.handler.{HttpHandler, WebSocketHandler}

// Define a WebSocket handler
object EchoHandler extends WebSocketHandler {
  override def connect(exchange: HttpExchange, listener: WebSocketListener): Task[Unit] = {
    // Echo back any text message received
    listener.receive.text.attach { message =>
      listener.send.text @= s"Echo: $message"
    }
    Task.unit
  }
}

// Mount it in a server
object WsServer extends StaticHttpServer {
  override protected val handler: HttpHandler = filters(
    "ws" / "echo" / EchoHandler
  )
}
```

### Client-Side WebSocket

```scala
import rapid._
import spice.http.client.HttpClient
import spice.net._

val ws = HttpClient
  .url(url"ws://localhost:8080/ws/echo")
  .webSocket()

// Listen for messages
ws.receive.text.attach { message =>
  println(s"Received: $message")
}

// Connect and send
ws.connect().sync()
ws.send.text @= "Hello, WebSocket!"
```

## Content Types

Spice provides a `Content` abstraction for HTTP bodies:

```scala
import spice.http.content.Content
import spice.net.ContentType

// String content
val textContent = Content.string("Hello!", ContentType.`text/plain`)
// textContent: Content = StringContent(
//   value = "Hello!",
//   contentType = ContentType(type = "text", subType = "plain", extras = Map()),
//   lastModified = 1771770964071L
// )

// JSON content
import fabric._
val jsonContent = Content.json(obj("message" -> str("Hello"), "count" -> num(42)))
// jsonContent: Content = JsonContent(
//   json = {"message": "Hello", "count": 42},
//   compact = true,
//   contentType = ContentType(
//     type = "application",
//     subType = "json",
//     extras = Map()
//   ),
//   lastModified = 1771770964075L
// )
```

File content is also supported:

```scala
import spice.http.content.Content
import java.io.File

val fileContent = Content.file(new File("data.csv"))
```

## Cross-Platform (Scala.js)

The `spice-core` and `spice-client` modules cross-compile to Scala.js. The JS client uses `XMLHttpRequest` under the hood:

```scala
// In Scala.js code
libraryDependencies += "com.outr" %%% "spice-core" % "1.0.0"
libraryDependencies += "com.outr" %%% "spice-client" % "1.0.0"
```

URL parsing, content types, headers, and all core HTTP types work identically on both platforms.

## 1.0 Checklist

- [X] OpenAPI-focused DSL for endpoints
  - [X] Practical example application fully tested
- [X] Production middleware (CORS, auth, rate limiting, security headers)
- [X] Structured error handling with content negotiation
- [X] Multi-part TLD support in URL parsing
- [X] MDoc documentation
