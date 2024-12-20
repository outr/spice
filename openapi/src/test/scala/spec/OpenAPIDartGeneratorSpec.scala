package spec

import rapid._
import fabric.rw._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import spice.net._
import spice.openapi.generator.OpenAPIGeneratorConfig
import spice.openapi.generator.dart.OpenAPIDartGenerator
import spice.openapi.server.{OpenAPIHttpServer, RestService, Service}

class OpenAPIDartGeneratorSpec extends AnyWordSpec with Matchers {
  "OpenAPIDartGenerator" should {
    "generate Dart code for the server" in {
      val sourceFiles = OpenAPIDartGenerator.generate(Server.api, OpenAPIGeneratorConfig(
        "SortDirection" -> Set(
          "Ascending", "Descending"
        ),
        "NumEnum" -> Set(
          "One", "Two", "Three"
        ),
        "Winner" -> Set(
          ".", "X", "O"
        )
      ))
      sourceFiles should not be Nil
      sourceFiles.map(_.fileName).toSet should be(Set("list_request.dart", "num_enum.dart", "file_upload_response.dart", "reverse_response.dart", "winner.dart", "reverse_request.dart", "list_response.dart", "combine_request.dart", "auth.dart", "sort_direction.dart", "combine_response.dart", "status.dart", "service.dart"))
      val listRequestSource = sourceFiles.find(_.fileName == "list_request.dart").get
      listRequestSource.source should include("import 'sort_direction.dart';")
      listRequestSource.source should include("final SortDirection direction;")
      val sortDirectionSource = sourceFiles.find(_.fileName == "sort_direction.dart").get
      sortDirectionSource.source should include("@JsonValue('Ascending')")
      sortDirectionSource.source should include("@JsonValue('Descending')")
    }
  }

  object Server extends OpenAPIHttpServer {
    override def title: String = "Example Server"
    override def version: String = "1.0"
    override def services: List[Service] = List(
      listService
    )
  }

  private val list = List("Apple", "Banana", "Cherry", "Date", "Elderberry", "Fig", "Grape")
  private val listService = RestService[ListRequest, ListResponse](path"/list", "List results") { request =>
    val l = request.direction match {
      case SortDirection.Ascending => list
      case SortDirection.Descending => list.reverse
    }
    Task.pure(ListResponse(l))
  }

  case class ListRequest(direction: SortDirection)

  object ListRequest {
    implicit val rw: RW[ListRequest] = RW.gen
  }

  case class ListResponse(results: List[String])

  object ListResponse {
    implicit val rw: RW[ListResponse] = RW.gen
  }

  sealed trait SortDirection

  object SortDirection {
    implicit val rw: RW[SortDirection] = RW.enumeration(List(Ascending, Descending))

    case object Ascending extends SortDirection
    case object Descending extends SortDirection
  }
}