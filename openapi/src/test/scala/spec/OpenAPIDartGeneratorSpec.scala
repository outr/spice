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
import spice.openapi.OpenAPI
import spice.openapi.OpenAPIInfo
import spice.openapi.OpenAPIComponents
import spice.openapi.OpenAPISchema
import fabric.str

class OpenAPIDartGeneratorSpec extends AnyWordSpec with Matchers {
  "OpenAPIDartGenerator" should {
    "generate Dart code for the server" in {
      val generator = OpenAPIDartGenerator(Server.api, OpenAPIGeneratorConfig())
      val sourceFiles = generator.generate()
      sourceFiles should not be Nil
      generator.write(sourceFiles, Path.of("output"))
      sourceFiles.map(_.fileName).toSet should be(Set("open_a_p_i_dart_generator_spec_filter.dart", "open_a_p_i_dart_generator_spec_list_response.dart", "open_a_p_i_dart_generator_spec_sort_direction.dart", "service.dart", "open_a_p_i_dart_generator_spec_list_request.dart"))
      val listRequestSource = sourceFiles.find(_.fileName == "open_a_p_i_dart_generator_spec_list_request.dart").get
      listRequestSource.source should include("import 'open_a_p_i_dart_generator_spec_sort_direction.dart';")
      listRequestSource.source should include("final OpenAPIDartGeneratorSpecSortDirection direction;")
      val sortDirectionSource = sourceFiles.find(_.fileName == "open_a_p_i_dart_generator_spec_sort_direction.dart").get
      sortDirectionSource.source should include("@JsonValue('Ascending')")
      sortDirectionSource.source should include("@JsonValue('Descending')")
    }
  }

  "should not generate Dart files for primitive types without additional properties" in {
    val api = OpenAPI(
      info = OpenAPIInfo(
        title = "Primitive Type Test",
        version = "1.0.0"
      ),
      paths = Map.empty,
      components = Some(OpenAPIComponents(
        schemas = Map(
          "SimpleString" -> OpenAPISchema.Component(`type` = "string"),
          "SimpleInteger" -> OpenAPISchema.Component(`type` = "integer"),
          "StringWithEnum" -> OpenAPISchema.Component(
            `type` = "string",
            `enum` = List(str("A"), str("B"), str("C"))
          ),
          "ComplexObject" -> OpenAPISchema.Component(
            `type` = "object",
            properties = Map(
              "name" -> OpenAPISchema.Component(`type` = "string"),
              "age" -> OpenAPISchema.Component(`type` = "integer")
            )
          )
        )
      ))
    )

    val config = OpenAPIGeneratorConfig()
    val generator = OpenAPIDartGenerator(api, config)
    val result = generator.generate()

    // Should only generate files for schemas that are not just primitive types
    result should have size 3 // Service + StringWithEnum + ComplexObject
    
    // Verify that primitive types without additional properties don't generate files
    val primitiveTypeFiles = result.filter(file => 
      file.fileName == "simple_string.dart" || file.fileName == "simple_integer.dart"
    )
    primitiveTypeFiles should have size 0
    
    // Verify that schemas with enums still generate files
    val enumFiles = result.filter(_.fileName == "string_with_enum.dart")
    enumFiles should have size 1
    
    // Verify that complex objects still generate files
    val objectFiles = result.filter(_.fileName == "complex_object.dart")
    objectFiles should have size 1
  }

  "should handle schema named 'string' correctly" in {
    val api = OpenAPI(
      info = OpenAPIInfo(
        title = "String Schema Test",
        version = "1.0.0"
      ),
      paths = Map.empty,
      components = Some(OpenAPIComponents(
        schemas = Map(
          "string" -> OpenAPISchema.Component(
            `type` = "string",
            description = Some("A string schema named 'string'")
          )
        )
      ))
    )

    val config = OpenAPIGeneratorConfig()
    val generator = OpenAPIDartGenerator(api, config)
    val result = generator.generate()

    // This should generate only 1 file: service.dart
    // The schema named "string" should be filtered out because it conflicts with Dart's built-in types
    result should have size 1
    
    val stringFiles = result.filter(_.fileName == "string.dart")
    stringFiles should have size 0
    
    // Only the service file should be generated
    val serviceFiles = result.filter(_.fileName == "service.dart")
    serviceFiles should have size 1
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