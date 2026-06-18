package spec

import fabric.str
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import spice.http.HttpMethod
import spice.net.ContentType
import spice.openapi.{OpenAPI, OpenAPIComponents, OpenAPIContent, OpenAPIContentType, OpenAPIInfo, OpenAPIPath, OpenAPIPathEntry, OpenAPIRequestBody, OpenAPIResponse, OpenAPISchema}
import spice.openapi.generator.OpenAPIGeneratorConfig
import spice.openapi.generator.kotlin.OpenAPIKotlinGenerator

class OpenAPIKotlinGeneratorSpec extends AnyWordSpec with Matchers {
  private def comp(props: (String, OpenAPISchema)*): OpenAPISchema.Component =
    OpenAPISchema.Component(`type` = "object", properties = props.toMap,
      required = props.map(_._1).toList, xFullClass = None)

  private def jsonContent(ref: String): OpenAPIContent =
    OpenAPIContent(ContentType.`application/json` -> OpenAPIContentType(schema = Some(OpenAPISchema.Ref(ref))))

  private val api = OpenAPI(
    info = OpenAPIInfo(title = "Kotlin Gen Test", version = "1.0.0"),
    paths = Map("/api/preview" -> OpenAPIPath(methods = Map(HttpMethod.Post -> OpenAPIPathEntry(
      summary = "Preview", description = "Preview",
      requestBody = Some(OpenAPIRequestBody(required = true, content = jsonContent("#/components/schemas/MoviePreview"))),
      responses = Map("200" -> OpenAPIResponse(description = "OK", content = Some(jsonContent("#/components/schemas/MediaPreview"))))
    )))),
    components = Some(OpenAPIComponents(schemas = Map(
      "SortOrder" -> OpenAPISchema.Component(`type` = "string", `enum` = List(str("popularity"), str("toprated"))),
      "MoviePreview" -> comp(
        "tmdbId" -> OpenAPISchema.Component(`type` = "integer"),
        "title" -> OpenAPISchema.Component(`type` = "string"),
        "runtime" -> OpenAPISchema.Component(`type` = "integer", nullable = Some(true))
      ),
      "ShowPreview" -> comp(
        "tmdbId" -> OpenAPISchema.Component(`type` = "integer"),
        "title" -> OpenAPISchema.Component(`type` = "string"),
        "seasonCount" -> OpenAPISchema.Component(`type` = "integer", nullable = Some(true))
      ),
      "MediaPreview" -> OpenAPISchema.OneOf(
        schemas = List(OpenAPISchema.Ref("#/components/schemas/MoviePreview"),
                       OpenAPISchema.Ref("#/components/schemas/ShowPreview")),
        discriminator = Some(OpenAPISchema.Discriminator("type",
          Map("MoviePreview" -> "#/components/schemas/MoviePreview",
              "ShowPreview" -> "#/components/schemas/ShowPreview")))
      )
    )))
  )

  "OpenAPIKotlinGenerator" should {
    val files = OpenAPIKotlinGenerator(api, OpenAPIGeneratorConfig(), basePackage = "tv.nabo.app.api").generate()
    def src(name: String): String = files.find(_.name == name).map(_.source)
      .getOrElse(throw new RuntimeException(s"$name not generated; got ${files.map(_.name)}"))

    "emit a sealed interface for the oneOf parent with common fields" in {
      val s = src("MediaPreview")
      s should include("sealed interface MediaPreview")
      s should include("val tmdbId: Int")
      s should include("val title: String")
      s should not include "runtime"   // movie-only, not common
    }
    "emit children implementing the parent with the wire @SerialName" in {
      val movie = src("MoviePreview")
      movie should include("@SerialName(\"MoviePreview\")")
      movie should include("data class MoviePreview(")
      movie should include(": MediaPreview")
      movie should include("override val tmdbId: Int")
      movie should include("val runtime: Int? = null")
    }
    "emit an enum with @SerialName values" in {
      val s = src("SortOrder")
      s should include("enum class SortOrder")
      s should include("@SerialName(\"popularity\") popularity")
    }
    "emit a Service with a typed suspend fun per path" in {
      val s = src("Service")
      s should include("object Service")
      s should include("suspend fun apiPreview(request: MoviePreview): MediaPreview")
      s should include("MediaPreview.serializer()")
    }
    "place models in the model package" in {
      files.filter(_.name != "Service").foreach(_.path should be("tv/nabo/app/api/model"))
    }
  }
}
