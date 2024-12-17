package spec

import fabric._
import fabric.io.{JsonFormatter, JsonParser}
import fabric.rw._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.{AnyWordSpec, AsyncWordSpec}
import spice.http.content.JsonContent
import spice.http.server.config.HttpsServerListener
import spice.http.{HttpExchange, HttpMethod, HttpRequest}
import spice.net._
import spice.openapi.server.{OpenAPIHttpServer, Service, ServiceCall}

import java.nio.file.{Files, Paths}

class OpenAPIServerSpec extends AnyWordSpec with Matchers {
  "OpenAPIServer" should {
    "validate a proper swagger.yml file" in {
      val expected = TestUtils.loadYaml("openapi-simple.yml")
      val json = SimpleOpenAPIServer.api.asJson
      Files.writeString(Paths.get("out.json"), JsonFormatter.Default(json))
      Files.writeString(Paths.get("expected.json"), JsonFormatter.Default(expected))
      json should be(expected)
    }
    "call the /users endpoint on api.example.com" in {
      val request = HttpRequest(url = url"https://api.example.com/v1/users")
      SimpleOpenAPIServer.handle(HttpExchange(request)).flatMap { exchange =>
        exchange.response.content.get.asString.map { contentString =>
          val json = JsonParser(contentString)
          json should be(arr(
            "root", "john.doe"
          ))
        }
      }.sync()
    }
    "call the /users endpoint on staging-api.example.com" in {
      val request = HttpRequest(url = url"https://staging-api.example.com/users")
      SimpleOpenAPIServer.handle(HttpExchange(request)).flatMap { exchange =>
        exchange.response.content.get.asString.map { jsonString =>
          val json = JsonParser(jsonString)
          json should be(arr(
            "root", "john.doe"
          ))
        }
      }.sync()
    }
  }

  object SimpleOpenAPIServer extends OpenAPIHttpServer {
    config
      .clearListeners()
      .addListeners(
        HttpsServerListener(
          host = "api.example.com",
          basePath = path"/v1",
          description = Some("Optional server description, e.g. Main (production) server")
        ),
        HttpsServerListener(
          host = "staging-api.example.com",
          description = Some("Optional server description, e.g. Internal staging server for testing")
        )
      )

    override val title: String = "Sample API"
    override val version: String = "0.1.9"
    override val description: Option[String] = Some("Optional multiline or single-line description in [CommonMark](https://commonmark.org/help/) or HTML.")

    object usersService extends Service {
      override val path: URLPath = path"/users"
      override val calls: List[ServiceCall] = List(
        serviceCall[Unit, List[String]](
          method = HttpMethod.Get,
          summary = "Returns a list of users.",
          description = "Optional extended description in CommonMark or HTML.",
          successDescription = "A JSON array of user names"
        ) { request =>
          request.response(List("root", "john.doe"))
        }
      )
    }

    override lazy val services: List[Service] = List(
      usersService
    )
  }
}