package spec

import fabric.io.JsonParser
import fabric.rw.*
import motn.model.*
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.io.Source

/** Deserializes a REAL response from the live API into the generated models.
  *
  * Compiling proves the generator emits valid Scala. This proves it emits the RIGHT Scala: the
  * fixture is an unedited `/v4/changes` response, so anything the generator got wrong about
  * optionality, naming or types fails here rather than in production.
  */
class ScalaGeneratedRoundTripSpec extends AnyWordSpec with Matchers {
  private def load(name: String): String = {
    val s = Source.fromInputStream(getClass.getClassLoader.getResourceAsStream(name), "UTF-8")
    try s.mkString finally s.close()
  }

  "the generated models" should {
    "deserialize a real /v4/changes response" in {
      val json = JsonParser(load("motn-changes-sample.json"))
      val result = json.as[ChangesResult]
      result.changes.size shouldBe 25
      result.shows.size should be > 0
    }
    "carry the episode-level detail the endpoint reports" in {
      val result = JsonParser(load("motn-changes-sample.json")).as[ChangesResult]
      val episodeChanges = result.changes.filter(_.itemType == ItemType.Episode)
      episodeChanges should not be empty
      // season/episode are optional in the schema and present for episode changes.
      episodeChanges.foreach { c =>
        c.season should be(defined)
        c.episode should be(defined)
        c.showId should not be empty
      }
    }
    "keep the link where the payload has one" in {
      val result = JsonParser(load("motn-changes-sample.json")).as[ChangesResult]
      result.changes.count(_.link.isDefined) should be > 0
    }
  }
}
