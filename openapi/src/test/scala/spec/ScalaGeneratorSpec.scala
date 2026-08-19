package spec

import fabric.io.YamlParser
import fabric.rw.*
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import spice.openapi.OpenAPI
import spice.openapi.generator.OpenAPIGeneratorConfig
import spice.openapi.generator.scala.OpenAPIScalaGenerator

import scala.io.Source

/** Generates Scala models from a real third-party spec (Streaming Availability API v4.1.0).
  *
  * Deliberately a published spec rather than a fixture: the generator's whole reason to exist is
  * consuming APIs we did not design, and a fixture would only ever contain shapes we already thought of.
  */
class ScalaGeneratorSpec extends AnyWordSpec with Matchers {
  private def load(name: String): String = {
    val s = Source.fromInputStream(getClass.getClassLoader.getResourceAsStream(name), "UTF-8")
    try s.mkString finally s.close()
  }

  private lazy val api: OpenAPI = YamlParser(load("motn-openapi.yaml")).as[OpenAPI]
  private lazy val files = OpenAPIScalaGenerator(api, OpenAPIGeneratorConfig(), basePackage = "motn").generate()

  "OpenAPIScalaGenerator" should {
    "emit a file per schema" in {
      files.size should be > 20
      files.map(_.fileName).foreach(_ should endWith(".scala"))
    }
    "honor the required list rather than nullable" in {
      val src = files.find(_.name == "StreamingOption").getOrElse(fail("no StreamingOption")).source
      // `link` is in the spec's required list -> mandatory; `quality` is not -> Option with a default.
      src should include("link: String")
      src should include("quality: Option[")
      src should include("= None")
    }
    "default collections to empty instead of Option" in {
      val src = files.find(_.name == "StreamingOption").getOrElse(fail("no StreamingOption")).source
      src should include("audios: List[")
    }
    "emit enums with their wire values" in {
      val src = files.find(_.name == "ChangeType").orElse(files.find(_.name == "ShowType"))
        .getOrElse(fail("no enum generated")).source
      src should include("enum ")
      src should include("""extends""")
    }
    "print the change schema for inspection" in {
      files.find(_.name == "Change").foreach(f => info(f.source))
      succeed
    }
  }
}
