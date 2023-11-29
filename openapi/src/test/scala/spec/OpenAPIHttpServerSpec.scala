package spec

import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import fabric.rw._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AsyncWordSpec
import spice.net._
import spice.openapi.server.{OpenAPIHttpServer, RestService, Service}

class OpenAPIHttpServerSpec extends AsyncWordSpec with AsyncIOSpec with Matchers {
  "OpenAPIHttpServer" should {
    "verify the YAML generated is correct" in {
      val expected = TestUtils.loadString("openapi-server.yaml")
      Example.api.asYaml should be(expected)
    }
  }

  object Example extends OpenAPIHttpServer {
    override def title: String = "Example Server"
    override def version: String = "1.0"
    override lazy val services: List[Service] = List(
      ReverseService, CombineService
    )
  }

  object ReverseService extends RestService[ReverseRequest, ReverseResponse] {
    override val path: URLPath = path"/reverse"

    override protected def summary: String = "Reverses text"

    override protected def apply(request: ReverseRequest): IO[ReverseResponse] = if (request.auth.username == "admin" && request.auth.password == "password") {
      IO.pure(ReverseResponse(Some(request.text.reverse), None))
    } else {
      IO.pure(ReverseResponse(None, Some("Invalid username/password combination")))
    }
  }

  object CombineService extends RestService[CombineRequest, CombineResponse] {
    override val path: URLPath = path"/combine"

    override protected def summary: String = "Combines the values of an enum"

    override protected def apply(request: CombineRequest): IO[CombineResponse] = if (request.auth.username == "admin" && request.auth.password == "password") {
      IO.pure(CombineResponse(request.map.values.toList.sortBy(_.getClass.getName), None))
    } else {
      IO.pure(CombineResponse(Nil, Some("Invalid username/password combination")))
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
}