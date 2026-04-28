package spec

import fabric.define.{DefType, Definition}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import spice.openapi.generator.dart.DartNames

/** Pins the contract of `DartNames` — the single source of truth for the
  * Scala-class → Dart-name conversion rules used by both the OpenAPI and
  * DurableSocket Dart generators. If the rules change, both generators must
  * change together. These tests fail loudly if anyone forks the logic into
  * a generator-local helper that drifts away from this contract. */
class DartNamesSpec extends AnyWordSpec with Matchers {
  "DartNames.stripTypeArgs" should {
    "strip a single type argument" in {
      DartNames.stripTypeArgs("Id[User]") shouldBe "Id"
    }
    "strip nested type arguments" in {
      DartNames.stripTypeArgs("Map[String, Id[User]]") shouldBe "Map"
    }
    "leave un-parameterized names alone" in {
      DartNames.stripTypeArgs("com.example.foo.Bar") shouldBe "com.example.foo.Bar"
    }
  }

  "DartNames.splitClassName" should {
    "split a typical FQN into package + class chain" in {
      DartNames.splitClassName("com.example.foo.Bar") shouldBe (List("com", "example", "foo"), List("Bar"))
    }
    "treat nested classes as part of the class chain" in {
      DartNames.splitClassName("spec.OpenAPIHttpServerSpec.Auth") shouldBe (List("spec"), List("OpenAPIHttpServerSpec", "Auth"))
    }
    "strip type arguments first" in {
      DartNames.splitClassName("lightdb.id.Id[User]") shouldBe (List("lightdb", "id"), List("Id"))
    }
    "drop Scala 3 anon and digit-only segments" in {
      DartNames.splitClassName("test.Status$$anon$1") shouldBe (List("test"), List("Status"))
    }
    "treat $ as a class-chain separator" in {
      DartNames.splitClassName("sigil.tool.model.ResponseContent$Text") shouldBe (List("sigil", "tool", "model"), List("ResponseContent", "Text"))
    }
  }

  "DartNames.dartClassName" should {
    "concatenate the class chain for nested cases" in {
      DartNames.dartClassName("sigil.tool.model.ResponseContent.Text") shouldBe "ResponseContentText"
    }
    "produce distinct names for cross-poly siblings (Sigil collision)" in {
      val a = DartNames.dartClassName("sigil.tool.model.ResponseContent.Text")
      val b = DartNames.dartClassName("sigil.conversation.ContextFrame.Text")
      a should not equal b
      a shouldBe "ResponseContentText"
      b shouldBe "ContextFrameText"
    }
    "return the leaf for top-level package classes" in {
      DartNames.dartClassName("lightdb.id.Id") shouldBe "Id"
    }
    "strip type arguments" in {
      DartNames.dartClassName("lightdb.id.Id[User]") shouldBe "Id"
    }
    "fall back to the leaf segment when there is no class chain" in {
      DartNames.dartClassName("Bar") shouldBe "Bar"
    }
  }

  "DartNames.dartSubtypeName" should {
    "use the className's class chain when present" in {
      val defn = Definition(DefType.Obj(Map.empty), className = Some("sigil.tool.model.ResponseContent.Text"))
      DartNames.dartSubtypeName("Text", defn, Some("ResponseContent")) shouldBe "ResponseContentText"
    }
    "fall back to parent + key for anonymous Scala 3 enum cases" in {
      val anon = Definition(DefType.Obj(Map.empty), className = Some("test.Status$$anon$1"))
      DartNames.dartSubtypeName("Active", anon, Some("Status")) shouldBe "StatusActive"
    }
    "fall back to just the key when no parent is provided" in {
      val anon = Definition(DefType.Obj(Map.empty), className = Some("test.Status$$anon$1"))
      DartNames.dartSubtypeName("Active", anon, None) shouldBe "Active"
    }
  }

  "DartNames.wireDiscriminator" should {
    "match Fabric's Product.productPrefix (simple leaf class name)" in {
      DartNames.wireDiscriminator("sigil.tool.model.ResponseContent.Text") shouldBe "Text"
      DartNames.wireDiscriminator("sigil.conversation.ContextFrame.Text") shouldBe "Text"
    }
    "return the same wire value for cross-poly siblings (the fact that broke 1.8.0)" in {
      DartNames.wireDiscriminator("sigil.tool.model.ResponseContent.Text") shouldBe DartNames.wireDiscriminator("sigil.conversation.ContextFrame.Text")
    }
    "strip type arguments" in {
      DartNames.wireDiscriminator("lightdb.id.Id[User]") shouldBe "Id"
    }
  }

  "DartNames.packagePath" should {
    "return slash-separated package segments" in {
      DartNames.packagePath("com.example.foo.Bar") shouldBe "com/example/foo"
      DartNames.packagePath("scalagentic.conversation.event.Deleted") shouldBe "scalagentic/conversation/event"
    }
    "return the package only (not the class chain)" in {
      DartNames.packagePath("spec.OpenAPIHttpServerSpec.Auth") shouldBe "spec"
    }
    "return an empty string for un-packaged names" in {
      DartNames.packagePath("Bar") shouldBe ""
    }
  }

  "DartNames.modelPathFor" should {
    "place classes under the package mirror by default" in {
      DartNames.modelPathFor("com.example.foo.Bar") shouldBe "lib/model/com/example/foo"
    }
    "fall back to the root for un-packaged names" in {
      DartNames.modelPathFor("Bar") shouldBe "lib/model"
    }
    "honor a custom root" in {
      DartNames.modelPathFor("com.example.foo.Bar", root = "lib/ws/durable/model") shouldBe "lib/ws/durable/model/com/example/foo"
    }
  }

  "DartNames.relativeImport" should {
    "return just the file name for the same directory" in {
      DartNames.relativeImport("lib/model/foo", "lib/model/foo", "bar.dart") shouldBe "bar.dart"
    }
    "walk up to a common ancestor and back down" in {
      DartNames.relativeImport("lib/model/a/b", "lib/model/c", "x.dart") shouldBe "../../c/x.dart"
    }
    "walk up only when the target is an ancestor" in {
      DartNames.relativeImport("lib/model/a/b", "lib/model", "x.dart") shouldBe "../../x.dart"
    }
    "walk down only when the source is an ancestor" in {
      DartNames.relativeImport("lib/model", "lib/model/a/b", "x.dart") shouldBe "a/b/x.dart"
    }
  }

  "DartNames.snakeCaseFile" should {
    "lowercase the first character and break before each subsequent uppercase letter" in {
      DartNames.snakeCaseFile("SimpleMessage") shouldBe "simple_message"
    }
    "treat acronym runs as separate letters (Dart's lowerCamel convention)" in {
      DartNames.snakeCaseFile("OpenAPIHttpServerSpecAuth") shouldBe "open_a_p_i_http_server_spec_auth"
    }
    "leave already-lowercase names alone" in {
      DartNames.snakeCaseFile("foo") shouldBe "foo"
    }
    "handle empty input" in {
      DartNames.snakeCaseFile("") shouldBe ""
    }
  }
}
