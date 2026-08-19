package spec

import fabric.io.YamlParser
import fabric.rw.*
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import spice.openapi.OpenAPI

import scala.io.Source

/** Can spice's OpenAPI model READ a spec it did not write?
  *
  * The generators consume `api: OpenAPI`, and every existing caller obtains that by asking spice to
  * describe its OWN services. Generating a client for a third-party API needs the opposite direction -
  * parse someone else's published spec into the same model - and nothing had ever established that
  * the model is complete enough to survive it. Uses the Streaming Availability API (v4.1.0) as a real
  * third-party spec rather than a hand-made fixture, because the point is exactly the constructs we
  * would not have thought to write ourselves.
  */
class ThirdPartySpecParseSpec extends AnyWordSpec with Matchers {
  private def load(name: String): String = {
    val s = Source.fromInputStream(getClass.getClassLoader.getResourceAsStream(name), "UTF-8")
    try s.mkString finally s.close()
  }

  "spice.openapi.OpenAPI" should {
    "parse a third-party published spec" in {
      val api = YamlParser(load("motn-openapi.yaml")).as[OpenAPI]
      api.info.title should include("Streaming Availability")
      api.components.map(_.schemas.size).getOrElse(0) should be > 20
    }
  }
}
