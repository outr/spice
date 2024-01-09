package spec

import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import fabric.rw._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AsyncWordSpec
import spice.http.server.rest.FileUpload
import spice.net._
import spice.openapi.generator.OpenAPIGeneratorConfig
import spice.openapi.generator.dart.OpenAPIDartGenerator
import spice.openapi.server.{OpenAPIHttpServer, RestService, Service}

import java.nio.file.{Files, Paths}

class OpenAPIHttpServerSpec extends AsyncWordSpec with AsyncIOSpec with Matchers {
  "OpenAPIHttpServer" should {
    "verify the YAML generated is correct" in {
//      Files.writeString(Paths.get("test.yaml"), Example.api.asYaml)
      val expected = TestUtils.loadString("openapi-server.yaml")
      Example.api.asYaml should be(expected)
    }
    "generate Dart code for the server" in {
      val sourceFiles = OpenAPIDartGenerator.generate(Example.api, OpenAPIGeneratorConfig())
      sourceFiles should not be Nil
      succeed
    }
  }

  object Example extends OpenAPIHttpServer {
    override def title: String = "Example Server"
    override def version: String = "1.0"
    override lazy val services: List[Service] = List(
      reverseService, combineService, fileUploadService
    )
  }

  private val reverseService = RestService[ReverseRequest, ReverseResponse](path"/reverse", "Reverses text") { request =>
    if (request.auth.username == "admin" && request.auth.password == "password") {
      IO.pure(ReverseResponse(Some(request.text.reverse), None))
    } else {
      IO.pure(ReverseResponse(None, Some("Invalid username/password combination")))
    }
  }

  private val combineService = RestService[CombineRequest, CombineResponse](path"/combine", "Combines the values of an enum") { request =>
    if (request.auth.username == "admin" && request.auth.password == "password") {
      IO.pure(CombineResponse(request.map.values.toList.sortBy(_.getClass.getName), None))
    } else {
      IO.pure(CombineResponse(Nil, Some("Invalid username/password combination")))
    }
  }

  private val fileUploadService = RestService[FileUploadRequest, FileUploadResponse](path"/upload", "Uploads a file") { request =>
    IO {
      val file = request.file
      scribe.info(s"Headers: ${file.headers}")
      FileUploadResponse(request.userId, file.file.length())
    }
  }

  case class ReverseRequest(auth: Auth, text: String)

  object ReverseRequest {
    implicit val rw: RW[ReverseRequest] = RW.gen
  }

  case class Auth(username: String, password: String)

  object Auth {
    implicit val rw: RW[Auth] = RW.gen
  }

  case class ReverseResponse(text: Option[String], error: Option[String])

  object ReverseResponse {
    implicit val rw: RW[ReverseResponse] = RW.gen
  }

  case class CombineRequest(auth: Auth, map: Map[String, NumEnum])

  object CombineRequest {
    implicit val rw: RW[CombineRequest] = RW.gen
  }

  sealed trait NumEnum

  object NumEnum {
    implicit val rw: RW[NumEnum] = RW.enumeration[NumEnum](List(One, Two, Three))

    case object One extends NumEnum
    case object Two extends NumEnum
    case object Three extends NumEnum
  }

  case class CombineResponse(list: List[NumEnum], error: Option[String])

  object CombineResponse {
    implicit val rw: RW[CombineResponse] = RW.gen
  }

  case class FileUploadRequest(userId: String, file: FileUpload)

  object FileUploadRequest {
    implicit val rw: RW[FileUploadRequest] = RW.gen
  }

  case class FileUploadResponse(userId: String, length: Long)

  object FileUploadResponse {
    implicit val rw: RW[FileUploadResponse] = RW.gen
  }
}