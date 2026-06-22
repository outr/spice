package spec

import fabric.define.{DefType, Definition, GenericType}
import fabric.rw.*
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import spice.openapi.generator.dart.{DurableSocketDartConfig, DurableSocketDartGenerator}

// AnyVal wrappers must be top-level (can't be nested in a class)
case class UserId(value: String) extends AnyVal
object UserId {
  given RW[UserId] = RW.string(_.value, UserId.apply, className = Some("test.UserId"))
}
case class CreatedAt(value: Long) extends AnyVal
object CreatedAt {
  given RW[CreatedAt] = RW.long(_.value, CreatedAt.apply, className = Some("test.CreatedAt"))
}

/** Tests Dart code generation from DefType — verifies the generator produces
  * correct Dart for enums, classes, polymorphic types, and edge cases. */
class DurableDefTypeSpec extends AnyWordSpec with Matchers {

  // --- Test types ---

  enum Color { case Red, Green, Blue }
  object Color {
    // Scala 3 parameterless enum cases share a single anonymous JVM class (`$$anon$1`), so the
    // default `asString` collapses every case onto one discriminator key. Use `productPrefix` to
    // give each case its declared name (`"Red"` / `"Green"` / `"Blue"`).
    given RW[Color] = RW.enumeration(Color.values.toList, asString = _.productPrefix)
  }

  case class SimpleMessage(text: String, count: Int) derives RW

  sealed trait Animal derives RW
  case class Dog(name: String, breed: String) extends Animal derives RW
  case class Cat(name: String, indoor: Boolean) extends Animal derives RW

  // Empty case objects (like ActionKind.Respond, MessageVisibility.All)
  enum Status derives RW {
    case Active
    case Inactive
    case Pending(reason: String)
  }

  // Nested types — a poly containing another poly
  case class Wrapper(animal: Animal, status: Status, timestamp: Long) derives RW

  // A poly whose child is ITSELF a poly — mirrors `LookupOutput extends
  // ToolOutput` in Sigil. `Win` is a direct (object) case; `Lookup` is a nested
  // sealed hierarchy whose leaves serialize with a DOTTED discriminator
  // (`...Lookup.Found`). The bare `...Lookup` therefore never appears on the
  // wire, so the parent `Outcome.fromJson` must prefix-match `...Lookup.` and
  // delegate — an exact `== '...Lookup'` falls through to the `Unknown` throw.
  sealed trait Outcome derives RW
  case class Win(score: Int) extends Outcome derives RW
  enum Lookup extends Outcome derives RW {
    case Found(name: String)
    case Missing(reason: String)
  }

  case class TypedRecord(id: UserId, name: String, created: CreatedAt, parentId: Option[UserId]) derives RW

  // Sigil bug #178 — nested companion enum. Pre-fix:
  //   - `toEmit` registered the enum under leaf name `Reason`.
  //   - Field-type rendering emitted `Reason` (leaf).
  //   - Import collection emitted `OwnerChange` qualified to `OwnerChangeReason`.
  //   The three names disagreed → Dart import target didn't exist + field
  //   type was unqualified, producing `InvalidType` cascades.
  case class OwnerChange(previousTier: Option[OwnerChange.Reason], newTier: OwnerChange.Reason) derives RW
  object OwnerChange {
    enum Reason derives RW {
      case Pinned
      case Repinned
      case Unpinned
    }
  }

  private def generateFiles(name: String, defn: Definition) = {
    val config = DurableSocketDartConfig(
      serviceName = "Test",
      wireType = name -> defn
    )
    DurableSocketDartGenerator(config).generate()
  }

  private def findSource(files: List[spice.openapi.generator.SourceFile], fileName: String): String =
    files.find(_.fileName == fileName).map(_.source).getOrElse(
      throw new NoSuchElementException(s"File $fileName not found in: ${files.map(_.fileName).mkString(", ")}")
    )

  private def allSources(files: List[spice.openapi.generator.SourceFile]): String =
    files.map(_.source).mkString("\n")

  // --- Tests ---

  "DurableDefTypeSpec" should {
    "generate Dart enum from DefType.Enum" in {
      val files = generateFiles("Color", summon[RW[Color]].definition)
      val source = findSource(files, "color.dart")
      source should include("enum Color")
      source should include("red,")
      source should include("green,")
      source should include("blue;")
      source should include("static Color? fromString")
    }

    "generate Dart class with fromJson constructor" in {
      val files = generateFiles("SimpleMessage", summon[RW[SimpleMessage]].definition)
      val source = findSource(files, "simple_message.dart")
      source should include("class SimpleMessage")
      source should include("final String text;")
      source should include("final int count;")
      source should include("SimpleMessage.fromJson(Map<String, dynamic> json)")
      source should include(": text =")
      source should not include ";;"
    }

    "generate Dart abstract class with polymorphic fromJson for sealed trait" in {
      val files = generateFiles("Animal", summon[RW[Animal]].definition)
      val all = allSources(files)
      all should include("abstract class Animal")
      all should include("static Animal fromJson")
      // Test types are nested in `DurableDefTypeSpec`, so the FQN class chain — both the Dart
      // class name and the wire `"type"` discriminator — carries the enclosing chain.
      all should include("if (type == 'DurableDefTypeSpec.Dog') return DurableDefTypeSpecDog.fromJson(json);")
      all should include("if (type == 'DurableDefTypeSpec.Cat') return DurableDefTypeSpecCat.fromJson(json);")
      all should include("class DurableDefTypeSpecDog extends Animal")
      all should include("class DurableDefTypeSpecCat extends Animal")
    }

    "generate proper class names for empty case objects in enums" in {
      val files = generateFiles("Status", summon[RW[Status]].definition)
      val all = allSources(files)
      all should not include "class 1"
      // Parameterless enum cases (Active/Inactive) get a stable `Definition.className` from
      // fabric's `derives RW` macro (`<EnumFQN>.<Case>`), so they share the same enclosing class
      // chain as the parameterized Pending case.
      all should include("class DurableDefTypeSpecStatusActive")
      all should include("class DurableDefTypeSpecStatusInactive")
      all should include("class DurableDefTypeSpecStatusPending")
      all should include("if (type == 'DurableDefTypeSpec.Status.Active') return DurableDefTypeSpecStatusActive.fromJson(json);")
    }

    "route a nested-poly child's dotted leaf discriminator via prefix match (LookupOutput.Found regression)" in {
      val files = generateFiles("Outcome", summon[RW[Outcome]].definition)
      val all = allSources(files)

      // The nested poly is its own abstract class that resolves its own leaves.
      all should include("abstract class DurableDefTypeSpecLookup")
      all should include("if (type == 'DurableDefTypeSpec.Lookup.Found') return DurableDefTypeSpecLookupFound.fromJson(json);")

      // The PARENT must delegate ANY `...Lookup.*` leaf to Lookup.fromJson. The
      // wire never carries the bare `...Lookup`, so without the prefix branch
      // `Lookup.Found` falls through to `throw ArgumentError('Unknown Outcome
      // type ...')` — the exact crash that froze the web client on
      // `LookupOutput.Found`.
      all should include("type?.startsWith('DurableDefTypeSpec.Lookup.')")
      all should include("return DurableDefTypeSpecLookup.fromJson(json);")

      // The direct (non-poly) child still uses an exact match — no prefix branch.
      all should include("if (type == 'DurableDefTypeSpec.Win') return DurableDefTypeSpecWin.fromJson(json);")
    }

    "decode the on<WireType> stream per-event so one bad event can't kill the subscription" in {
      val config = DurableSocketDartConfig(
        serviceName = "Test",
        wireType = "Animal" -> summon[RW[Animal]].definition
      )
      val files = DurableSocketDartGenerator(config).generate()
      val client = files.find(_.fileName == "test_durable_client.dart").get.source

      // Per-event decode that catches + skips, not a bare `.map` that lets a
      // single undecodable event error the whole stream (which silently stops
      // ALL further events and hangs in-flight work forever).
      client should include("expand((e) sync*")
      client should include("yield (e.")
      client should include("catch (error, stack)")
      client should not include "_eventController.stream.map((e) => (e."
      // The newline in the log line must be a literal `\n` escape, not a real
      // newline — a real one breaks the single-quoted Dart string (build_runner
      // parse error). This pins the escape so the closing `')` stays on-line.
      client should include("""skipped undecodable event: $error\n$stack')""")
    }

    "recursively discover nested types" in {
      val files = generateFiles("Wrapper", summon[RW[Wrapper]].definition)
      val all = allSources(files)
      // The top-level type uses the explicit name passed to generateFiles ("Wrapper").
      // Discovered nested types derive their names from the FQN class chain.
      all should include("class Wrapper")
      all should include("abstract class Animal")
      all should include("class DurableDefTypeSpecDog")
      all should include("class DurableDefTypeSpecCat")
      all should include("abstract class Status")
    }

    "not generate invalid Dart identifiers (no 'enum String')" in {
      val files = generateFiles("Wrapper", summon[RW[Wrapper]].definition)
      val all = allSources(files)
      all should not include "enum String {"
    }

    "generate fromJson with .map for List<Poly> and null-check for Optional<Poly>" in {
      val files = generateFiles("Wrapper", summon[RW[Wrapper]].definition)
      val all = allSources(files)
      all should not include ".cast<Animal>()"
      all should not include ".cast<Status>()"
      all should include("Animal.fromJson")
      all should include("Status.fromJson")
    }

    "generate typed Dart wrapper classes for AnyVal types with className" in {
      val files = generateFiles("TypedRecord", summon[RW[TypedRecord]].definition)
      val all = allSources(files)

      all should include("class UserId {")
      all should include("final String value;")
      all should include("const UserId(this.value);")

      all should include("class CreatedAt {")
      all should include("final int value;")
      all should include("const CreatedAt(this.value);")

      all should include("final UserId id;")
      all should include("final CreatedAt created;")
      all should include("final UserId? parentId;")

      all should include("UserId(")
      all should include("CreatedAt(")

      all should include("id.value")
      all should include("created.value")
    }

    "generate valid Dart constructor syntax" in {
      val files = generateFiles("SimpleMessage", summon[RW[SimpleMessage]].definition)
      val source = findSource(files, "simple_message.dart")
      val lines = source.split("\n")
      lines.count(_.trim.endsWith(";;")) shouldBe 0
      val fromJsonLine = lines.find(_.contains("SimpleMessage.fromJson"))
      fromJsonLine should not be empty
    }

    "generate generic Dart types for parameterized wrappers like Id[User]" in {
      // Simulate what Fabric 1.24 produces for a field of type Id[User]
      // where Id[T] is case class Id[T](value: String)
      val userDef = Definition(
        DefType.Obj(Map("name" -> Definition(DefType.Str))),
        className = Some("test.User")
      )
      val idUserDef = Definition(
        DefType.Str,
        className = Some("test.Id[User]"),
        genericTypes = List(GenericType("T", userDef))
      )
      val idPostDef = Definition(
        DefType.Str,
        className = Some("test.Id[Post]"),
        genericTypes = List(GenericType("T", Definition(
          DefType.Obj(Map("title" -> Definition(DefType.Str))),
          className = Some("test.Post")
        )))
      )
      val recordDef = Definition(
        DefType.Obj(Map(
          "userId" -> idUserDef,
          "postId" -> idPostDef,
          "name" -> Definition(DefType.Str)
        )),
        className = Some("test.Record")
      )
      val files = generateFiles("Record", recordDef)
      val all = allSources(files)

      // Should generate ONE Id wrapper class (not Id[User] and Id[Post] separately)
      all should include("class Id {")
      all should include("final String value;")
      all should include("const Id(this.value);")

      // Field types should use Dart generics
      val recordSource = findSource(files, "record.dart")
      recordSource should include("final Id<User> userId;")
      recordSource should include("final Id<Post> postId;")
      recordSource should include("final String name;")
    }

    "generate typed push and on<WireType> stream from wireType" in {
      val config = DurableSocketDartConfig(
        serviceName = "Test",
        wireType = "Animal" -> summon[RW[Animal]].definition
      )
      val files = DurableSocketDartGenerator(config).generate()
      val client = files.find(_.fileName == "test_durable_client.dart").get.source

      // Typed push method — only typed API exists
      client should include("int push(Animal animal)")
      client should include("'data': animal.toJson()")
      // Typed stream getter
      client should include("Stream<(int, Animal)> get onAnimal")
      client should include("Animal.fromJson(e.")
      // Imports the types file
      client should include("import 'test_types.dart';")

      // Untyped escape hatches removed
      client should not include "int pushRaw"
      client should not include "Stream<(int, Map<String, dynamic>)> get onEvent"
      client should not include "int push(Map<String, dynamic>"

      // Wire type is auto-generated even though it wasn't in defTypes
      val all = allSources(files)
      all should include("abstract class Animal")
      // Subtypes' Dart names include the test class wrapper from the FQN class chain
      all should include("class DurableDefTypeSpecDog extends Animal")
      all should include("class DurableDefTypeSpecCat extends Animal")
    }

    "qualify cross-poly nested case classes (Sigil-style Text/Image collision)" in {
      // Two distinct polymorphic parents each declare a `Text` case nested in their
      // companion. With the class-chain rule, they MUST produce distinct Dart class
      // names (ResponseContentText / ContextFrameText) and not collapse onto a single
      // class — that was the cross-poly collision reported by downstream consumers.
      // Wire format stays simple ("Text" via Fabric's productPrefix).
      val responseContentDef = Definition(
        DefType.Poly(scala.collection.immutable.VectorMap(
          "Text" -> Definition(
            DefType.Obj(scala.collection.immutable.VectorMap("text" -> Definition(DefType.Str))),
            className = Some("sigil.tool.model.ResponseContent.Text")
          ),
          "Image" -> Definition(
            DefType.Obj(scala.collection.immutable.VectorMap("url" -> Definition(DefType.Str))),
            className = Some("sigil.tool.model.ResponseContent.Image")
          )
        )),
        className = Some("sigil.tool.model.ResponseContent")
      )
      val contextFrameDef = Definition(
        DefType.Poly(scala.collection.immutable.VectorMap(
          "Text" -> Definition(
            DefType.Obj(scala.collection.immutable.VectorMap("content" -> Definition(DefType.Str))),
            className = Some("sigil.conversation.ContextFrame.Text")
          ),
          "System" -> Definition(
            DefType.Obj(scala.collection.immutable.VectorMap("level" -> Definition(DefType.Str))),
            className = Some("sigil.conversation.ContextFrame.System")
          )
        )),
        className = Some("sigil.conversation.ContextFrame")
      )
      // Wrap both polys in a single root so both are processed
      val rootDef = Definition(
        DefType.Obj(scala.collection.immutable.VectorMap(
          "content" -> responseContentDef,
          "frame" -> contextFrameDef
        )),
        className = Some("test.Root")
      )
      val config = DurableSocketDartConfig(
        serviceName = "Test",
        wireType = "Root" -> rootDef
      )
      val files = DurableSocketDartGenerator(config).generate()
      val all = allSources(files)

      // Distinct Dart class names — no collision
      all should include("class ResponseContentText extends ResponseContent")
      all should include("class ResponseContentImage extends ResponseContent")
      all should include("class ContextFrameText extends ContextFrame")
      all should include("class ContextFrameSystem extends ContextFrame")

      // Each parent's dispatch resolves to its OWN child, not the sibling's
      all should include("if (type == 'Text') return ResponseContentText.fromJson(json);")
      all should include("if (type == 'Text') return ContextFrameText.fromJson(json);")

      // Wire format unchanged — still just the simple case name
      all should include("'type': 'Text'")
    }

    "import inner types of List/Map commonFields on the abstract parent (bug #47)" in {
      // Bug #47 reproducer — a polytype subtype with `tags: List[Tag]`,
      // `meta: Map[String, MetaValue]`, and `parent: Tag` (bare). With one
      // subtype registered, every field becomes a common field via the
      // intersection. Pre-fix, the import collector dropped `List<...>` /
      // `Map<...>` entries on the prefix without recursing — so `Tag`
      // arrived (from `parent`) but `MetaValue` (only reachable via the
      // Map's value type) didn't.
      val tagDef = Definition(DefType.Str, className = Some("test.tag.Tag"))
      val metaValueDef = Definition(
        DefType.Obj(scala.collection.immutable.VectorMap("name" -> Definition(DefType.Str))),
        className = Some("test.meta.MetaValue")
      )
      val recordDef = Definition(
        DefType.Obj(scala.collection.immutable.VectorMap(
          "tags"   -> Definition(DefType.Arr(tagDef)),
          "meta"   -> Definition(DefType.Obj(scala.collection.immutable.VectorMap("__map__" -> metaValueDef))),
          "parent" -> tagDef
        )),
        className = Some("test.poly.RecordSubtype")
      )

      // Use `defTypeToDartType` indirectly — we need a Map<String, T>
      // shape, which spice emits when the inner Obj has the special
      // `__map__` key. If that's not the convention, switch to a
      // straight Obj — the bug repro still hits via `tags` alone, the
      // Map case just adds extra coverage.
      val polyDef = Definition(
        DefType.Poly(
          values       = scala.collection.immutable.VectorMap("RecordSubtype" -> recordDef),
          commonFields = recordDef.defType.asInstanceOf[DefType.Obj].map.toMap
        ),
        className = Some("test.poly.Record")
      )
      val rootDef = Definition(
        DefType.Obj(scala.collection.immutable.VectorMap("payload" -> polyDef)),
        className = Some("test.Root")
      )
      val files = DurableSocketDartGenerator(
        DurableSocketDartConfig(serviceName = "Test", wireType = "Root" -> rootDef)
      ).generate()

      val recordFile = findSource(files, "record.dart")
      // The abstract parent must import `Tag` so its `List<Tag>?`
      // (and `parent: Tag`) getters resolve.
      recordFile should include regex """import\s+'[^']*tag\.dart';"""
      // Sanity: the abstract class itself uses the type.
      recordFile should include("abstract class Record")
      recordFile should include regex """List<Tag>\??\s+get\s+tags;"""
    }

    "import every typed class referenced by polytype commonFields, including from sibling packages (bug #49)" in {
      // Bug #49 reproducer — a polytype `Participant` in `sigil.participant`
      // whose subtype `DefaultAgentParticipant` references types living
      // in sibling packages: `BuiltInTool` in `sigil.provider`, `Role`
      // in `sigil.role`, plus a bare typed class field `instructions:
      // Instructions` (also in `sigil.provider`) and a `List<X>` of
      // each. The abstract parent emits getters for all of them and
      // must import every type, regardless of which package it lives
      // in or whether it's wrapped in `List<>` / `Option[]`.
      val builtInToolDef = Definition(
        DefType.Str,
        className = Some("sigil.provider.BuiltInTool")
      )
      val roleDef = Definition(
        DefType.Obj(scala.collection.immutable.VectorMap("name" -> Definition(DefType.Str))),
        className = Some("sigil.role.Role")
      )
      val instructionsDef = Definition(
        DefType.Obj(scala.collection.immutable.VectorMap("text" -> Definition(DefType.Str))),
        className = Some("sigil.provider.Instructions")
      )
      val agentDef = Definition(
        DefType.Obj(scala.collection.immutable.VectorMap(
          "displayName"   -> Definition(DefType.Str),
          "builtInTools"  -> Definition(DefType.Arr(builtInToolDef)),
          "roles"         -> Definition(DefType.Arr(roleDef)),
          "instructions"  -> Definition(DefType.Opt(instructionsDef))
        )),
        className = Some("sigil.participant.DefaultAgentParticipant")
      )
      val participantDef = Definition(
        DefType.Poly(
          values       = scala.collection.immutable.VectorMap("DefaultAgentParticipant" -> agentDef),
          commonFields = agentDef.defType.asInstanceOf[DefType.Obj].map.toMap
        ),
        className = Some("sigil.participant.Participant")
      )
      val rootDef = Definition(
        DefType.Obj(scala.collection.immutable.VectorMap("payload" -> participantDef)),
        className = Some("test.Root")
      )
      val files = DurableSocketDartGenerator(
        DurableSocketDartConfig(serviceName = "Test", wireType = "Root" -> rootDef)
      ).generate()

      val baseFile = findSource(files, "participant.dart")
      // Every typed class referenced from a commonField (whether in
      // List[T], Option[T], or bare) must produce an import. We don't
      // pin specific paths — relative-path encoding can vary — but
      // each target file must appear in some import statement.
      baseFile should include regex """import\s+'[^']*built_in_tool\.dart';"""
      baseFile should include regex """import\s+'[^']*role\.dart';"""
      baseFile should include regex """import\s+'[^']*instructions\.dart';"""

      // Sanity — abstract base uses each type.
      baseFile should include("abstract class Participant")
      baseFile should include regex """List<BuiltInTool>\??\s+get\s+builtInTools;"""
      baseFile should include regex """List<Role>\??\s+get\s+roles;"""
      baseFile should include regex """Instructions\??\s+get\s+instructions;"""
    }

    "not double-nullable polytype commonFields that wrap Option[T] (bug #48)" in {
      // Reproducer for bug #48 — an open polytype whose ONLY subtype
      // (or every subtype) declares `Option[T]` fields. Before the
      // fix, fabric promoted the full `Definition(DefType.Opt(...))`
      // into `commonFields`, and the Dart generator unconditionally
      // appended `?` to `defTypeToDartType(...)` — which already
      // rendered the Opt as `T?`. Result: `T??` (invalid Dart) and
      // bogus `import '../../bool?.dart';` because the import path
      // was built from the Dart type name with the trailing `?`.
      val applicationStateDef = Definition(
        DefType.Obj(scala.collection.immutable.VectorMap(
          "sidebarCollapsed" -> Definition(DefType.Opt(Definition(DefType.Bool))),
          "theme"            -> Definition(DefType.Opt(Definition(DefType.Str)))
        )),
        className = Some("test.viewer.ApplicationState")
      )
      val viewerStatePayloadDef = Definition(
        DefType.Poly(scala.collection.immutable.VectorMap(
          "ApplicationState" -> applicationStateDef
        )),
        className = Some("test.viewer.ViewerStatePayload")
      )
      val rootDef = Definition(
        DefType.Obj(scala.collection.immutable.VectorMap("payload" -> viewerStatePayloadDef)),
        className = Some("test.Root")
      )
      val config = DurableSocketDartConfig(
        serviceName = "Test",
        wireType = "Root" -> rootDef
      )
      // Compute commonFields the way fabric's PolyType.register would
      // — single-subtype intersection is the whole subtype's field
      // map. Tests construct `DefType.Poly` directly, so we set
      // `commonFields` explicitly to mirror that.
      val polyWithCommon = viewerStatePayloadDef.copy(
        defType = DefType.Poly(
          values = applicationStateDef.defType.asInstanceOf[DefType.Obj].map.toMap match {
            case _ => scala.collection.immutable.VectorMap(
              "ApplicationState" -> applicationStateDef
            )
          },
          commonFields = applicationStateDef.defType.asInstanceOf[DefType.Obj].map.toMap
        )
      )
      val rootWithCommon = Definition(
        DefType.Obj(scala.collection.immutable.VectorMap("payload" -> polyWithCommon)),
        className = Some("test.Root")
      )
      val files = DurableSocketDartGenerator(
        DurableSocketDartConfig(serviceName = "Test", wireType = "Root" -> rootWithCommon)
      ).generate()
      val all = allSources(files)

      // The abstract parent's getters must be single-nullable.
      val abstractFile = findSource(files, "viewer_state_payload.dart")
      abstractFile should include("abstract class ViewerStatePayload")
      abstractFile should include("bool? get sidebarCollapsed;")
      abstractFile should include("String? get theme;")

      // The abstract parent's getter declarations must NOT use `??`.
      // (`??` legitimately appears elsewhere in the generated client
      // code as Dart's null-coalescing operator, so scope to the
      // poly file.)
      abstractFile should not include "??"

      // No bogus `T?.dart` imports for primitive-typed Option fields.
      abstractFile should not include "bool?.dart"
      abstractFile should not include "string?.dart"
      abstractFile should not include "?.dart"
    }

    "render fabric Json field as dynamic, not Map<String, dynamic> (text-only payloads break the cast)" in {
      // Bare `fabric.Json` field and `List[fabric.Json]` field. Each
      // can carry a String / number / bool / null / object / array, so
      // a Dart type of `Map<String, dynamic>` plus a `.cast<Map<String,
      // dynamic>>()` on read throws at runtime whenever the wire
      // payload happens to be a primitive — the failure mode for any
      // paginated-result envelope shipping file-contents-as-string
      // items.
      val recordDef = Definition(
        DefType.Obj(scala.collection.immutable.VectorMap(
          "items" -> Definition(DefType.Arr(Definition(DefType.Json))),
          "extra" -> Definition(DefType.Json)
        )),
        className = Some("test.JsonRecord")
      )
      val files = generateFiles("JsonRecord", recordDef)
      val source = findSource(files, "json_record.dart")

      // Field declarations: dynamic, not Map<String, dynamic>.
      source should include("final List<dynamic> items;")
      source should include("final dynamic extra;")
      source should not include "List<Map<String, dynamic>> items"
      source should not include "Map<String, dynamic> extra"

      // fromJson: no `.cast<Map<String, dynamic>>()` on the list,
      // no `as Map<String, dynamic>` on the bare field.
      source should not include "cast<Map<String, dynamic>>()"
      // The list pass-through still produces a `List<dynamic>` —
      // either a plain `as List?` fall-through or an explicit
      // `.cast<dynamic>()` is fine; what matters is the inner type
      // matches the field declaration.
      source should not include ".cast<Map<String, dynamic>>"
    }

    "Sigil bug #178 — emit qualified Dart enum for a nested companion enum" in {
      val files = generateFiles("OwnerChange", summon[RW[OwnerChange]].definition)
      val all = allSources(files)

      // The enum file is emitted under the qualified name (snake_case),
      // matching what the import and field-type now reference.
      val enumFile = files.find(_.fileName.endsWith("owner_change_reason.dart"))
      enumFile shouldBe defined
      enumFile.get.source should include("enum DurableDefTypeSpecOwnerChangeReason")

      // The parent class file references the qualified name, NOT the bare
      // leaf `Reason`. Pre-fix this used the leaf and Dart fell back to
      // `InvalidType`.
      val ownerFile = findSource(files, "owner_change.dart")
      ownerFile should include("DurableDefTypeSpecOwnerChangeReason newTier")
      ownerFile should not include "final Reason newTier"
      ownerFile should not include "InvalidType"

      // The import path resolves to the actually-emitted file name —
      // no `complexity_change_reason.dart` style orphan import.
      all should not include "InvalidType"
    }
  }
}
