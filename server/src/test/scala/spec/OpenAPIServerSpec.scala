package spec

import cats.effect.testing.scalatest.AsyncIOSpec
import fabric._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AsyncWordSpec
import spice.http.content.JsonContent
import spice.http.{HttpExchange, HttpRequest}
import spice.http.server.config.HttpServerListener
import spice.http.server.openapi.server.{Service, ServiceCall}
import spice.http.server.openapi._
import spice.net._

class OpenAPIServerSpec extends AsyncWordSpec with AsyncIOSpec with Matchers {
  "OpenAPIServer" should {
    "validate a proper swagger.yml file" in {
      val expected = TestUtils.loadYaml("openapi-simple.yml")
      val json = SimpleOpenAPIServer.api.asJson
      json should be(expected)
    }
    "call the /users endpoint on api.example.com" in {
      val request = HttpRequest(url = url"http://api.example.com/v1/users")
      SimpleOpenAPIServer.handle(HttpExchange(request)).map { exchange =>
        exchange.response.content.map(_.asInstanceOf[JsonContent].json) should be(Some(arr(
          "root", "john.doe"
        )))
      }
    }
    "call the /users endpoint on staging-api.example.com" in {
      val request = HttpRequest(url = url"http://staging-api.example.com/users")
      SimpleOpenAPIServer.handle(HttpExchange(request)).map { exchange =>
        exchange.response.content.map(_.asInstanceOf[JsonContent].json) should be(Some(arr(
          "root", "john.doe"
        )))
      }
    }
  }

  object SimpleOpenAPIServer extends server.OpenAPIServer {
    config
      .clearListeners()
      .addListeners(
        HttpServerListener(
          host = "api.example.com",
          port = 80,
          basePath = path"/v1",
          description = Some("Optional server description, e.g. Main (production) server")
        ),
        HttpServerListener(
          host = "staging-api.example.com",
          port = 80,
          description = Some("Optional server description, e.g. Internal staging server for testing")
        )
      )

    override val title: String = "Sample API"
    override val version: String = "0.1.9"
    override val description: Option[String] = Some("Optional multiline or single-line description in [CommonMark](http://commonmark.org/help/) or HTML.")

    object usersService extends Service {
      override val path: URLPath = path"/users"
      override val get: ServiceCall = serviceCall[Unit, List[String]](
        summary = "Returns a list of users.",
        description = "Optional extended description in CommonMark or HTML.",
        successDescription = "A JSON array of user names"
      ) { request =>
        request.response(List("root", "john.doe"))
      }
    }

    override val services: List[Service] = List(
      usersService
    )
  }
}