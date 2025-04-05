package spec

import rapid._
import fabric.rw._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import spice.net._
import spice.openapi.generator.OpenAPIGeneratorConfig
import spice.openapi.generator.dart.OpenAPIDartGenerator
import spice.openapi.server.{OpenAPIHttpServer, RestService, Service}

import java.nio.file.Path

class OpenAPIDartGeneratorSpec extends AnyWordSpec with Matchers {
  "OpenAPIDartGenerator" should {
    "generate Dart code for the server" in {
      val generator = OpenAPIDartGenerator(Server.api, OpenAPIGeneratorConfig())
      val sourceFiles = generator.generate()
      sourceFiles should not be Nil
      generator.write(sourceFiles, Path.of("output"))
      sourceFiles.map(_.fileName).toSet should be(Set("open_a_p_i_dart_generator_spec_filter.dart", "open_a_p_i_dart_generator_spec_list_response.dart", "sort_direction.dart", "service.dart", "open_a_p_i_dart_generator_spec_list_request.dart"))
      val listRequestSource = sourceFiles.find(_.fileName == "open_a_p_i_dart_generator_spec_list_request.dart").get
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
  private val listService = RestService[ListRequest, ListResponse](Server, path"/list", "List results") { request =>
    val l = request.direction match {
      case SortDirection.Ascending => list
      case SortDirection.Descending => list.reverse
    }
    val filtered = request.filter match {
      case Some(f) => l.filter(_.matches(f.regex))
      case None => l
    }
    Task.pure(ListResponse(filtered))
  }

  case class ListRequest(filter: Option[Filter], direction: SortDirection)

  object ListRequest {
    implicit val rw: RW[ListRequest] = RW.gen
  }

  case class ListResponse(results: List[String])

  object ListResponse {
    implicit val rw: RW[ListResponse] = RW.gen
  }

  case class Filter(regex: String)

  object Filter {
    implicit val rw: RW[Filter] = RW.gen
  }

  sealed trait SortDirection

  object SortDirection {
    implicit val rw: RW[SortDirection] = RW.enumeration(List(Ascending, Descending))

    case object Ascending extends SortDirection
    case object Descending extends SortDirection
  }
}