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
    "initialize the server" in {
      SimpleOpenAPIServer.init().sync()
    }
    "validate a proper swagger.yml file" in {
      val expected = TestUtils.loadYaml("openapi-standard.yml")
      val json = SimpleOpenAPIServer.api.asJson
      Files.writeString(Paths.get("out.json"), JsonFormatter.Default(json))
      Files.writeString(Paths.get("expected.json"), JsonFormatter.Default(expected))
      json should be(expected)
    }
    "call the /users endpoint on api.example.com" in {
      val request = HttpRequest(url = url"https://api.example.com/v1/users")
      SimpleOpenAPIServer.handle(HttpExchange(request)).flatMap { exchange =>
        exchange.response.content.get.asString.map { jsonString =>
          val json = JsonParser(jsonString)
          json.as[List[User]] should be(List(
            User("root", UserType.Admin),
            User("john.doe", UserType.Basic)
          ))
        }
      }.sync()
    }
    "call the /users endpoint on staging-api.example.com" in {
      val request = HttpRequest(url = url"https://staging-api.example.com/users")
      SimpleOpenAPIServer.handle(HttpExchange(request)).flatMap { exchange =>
        exchange.response.content.get.asString.map { jsonString =>
          val json = JsonParser(jsonString)
          json.as[List[User]] should be(List(
            User("root", UserType.Admin),
            User("john.doe", UserType.Basic)
          ))
        }
      }.sync()
    }
  }

  object SimpleOpenAPIServer extends OpenAPIHttpServer { s =>
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
      override def server: OpenAPIHttpServer = s
      override val path: URLPath = path"/users"
      override val calls: List[ServiceCall] = List(
        serviceCall[Unit, List[User]](
          method = HttpMethod.Get,
          summary = "Returns a list of users.",
          description = "Optional extended description in CommonMark or HTML.",
          successDescription = "A JSON array of users"
        ) { request =>
          request.response(List(
            User("root", UserType.Admin), User("john.doe", UserType.Basic)
          ))
        }
      )
    }

    override lazy val services: List[Service] = List(
      usersService
    )
  }

  case class User(name: String, `type`: UserType)

  object User {
    implicit val rw: RW[User] = RW.gen
  }

  sealed trait UserType

  object UserType {
    implicit val rw: RW[UserType] = RW.enumeration(List(Admin, Basic))

    case object Admin extends UserType
    case object Basic extends UserType
  }
}