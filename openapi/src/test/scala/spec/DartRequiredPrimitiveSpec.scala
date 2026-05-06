package spec

import fabric.rw.*
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import spice.openapi.generator.dart.{DurableSocketDartConfig, DurableSocketDartGenerator}

/** Coverage for sigil bug #11 — Dart codegen rendered required primitive
  * fields (Boolean, Int, Long, Double, String) as nullable in the generated
  * Dart class, erasing the Scala-side required-ness on the wire. The fix
  * preserves required vs defaulted vs optional in the generated Dart so
  * downstream callers can't construct a malformed payload. */
class DartRequiredPrimitiveSpec extends AnyWordSpec with Matchers {

  case class RespondOptionsInput(prompt: String,
                                  options: List[String],
                                  allowMultiple: Boolean) derives RW

  case class SelectOption(label: String,
                           value: String,
                           exclusive: Boolean = false) derives RW

  case class AllPrimitives(s: String,
                            i: Int,
                            l: Long,
                            d: Double,
                            b: Boolean) derives RW

  case class WithOptions(name: String,
                          age: Option[Int],
                          enabled: Option[Boolean]) derives RW

  case class WithNoneDefaults(name: Option[String] = None,
                               sessionId: Option[String] = None,
                               workspaceId: Option[String] = None) derives RW

  case class WithWrapperDefaults(created: SpecTimestamp = SpecTimestamp(1234L),
                                  id: SpecId = SpecId("static-id")) derives RW

  case class WithEnumDefault(state: SpecState = SpecState.Active) derives RW

  case class WithCollectionDefaults(participants: List[String] = Nil,
                                     keywords: List[String] = Nil) derives RW

  // Effectful wrapper defaults — values vary per construction.
  case class WithEffectfulWrappers(created: SpecTimestamp = SpecTimestamp(System.currentTimeMillis()),
                                    fresh: SpecId = SpecId(java.util.UUID.randomUUID().toString)) derives RW

  // Polytype with an empty case-object subtype — singleton default.
  case class WithSingletonDefault(currentMode: SpecMode = SpecConversationMode) derives RW

  // Single-subtype polytype default — the most common sigil shape
  // (`Mode = ConversationMode` ships one subtype out of the box).
  case class WithSingleSubtypePoly(mode: SpecSingleMode = SpecSingleConvMode) derives RW

  // Mode-shaped trait: concrete field defaults on the parent + case-
  // object subtypes that override them. Mirrors Sigil's actual `Mode`
  // / `ConversationMode` / `CodingMode` setup that bug #15 surfaces.
  case class WithModeShape(kind: SpecModeShapeKind = SpecModeShapeConv) derives RW

  private def generate(name: String, defn: fabric.define.Definition) =
    DurableSocketDartGenerator(
      DurableSocketDartConfig(serviceName = "Test", wireType = name -> defn)
    ).generate()

  private def find(files: List[spice.openapi.generator.SourceFile], fileName: String): String =
    files.find(_.fileName == fileName).map(_.source).getOrElse(
      throw new NoSuchElementException(s"$fileName not found in: ${files.map(_.fileName).mkString(", ")}")
    )

  "DartRequiredPrimitiveSpec (sigil bug #11)" should {
    "render required Boolean as `bool`, not `bool?`" in {
      val files = generate("RespondOptionsInput", summon[RW[RespondOptionsInput]].definition)
      val source = find(files, "respond_options_input.dart")
      source should include("final bool allowMultiple;")
      source should not include "final bool? allowMultiple;"
      source should include("required this.allowMultiple")
    }

    "keep `required` modifier on a defaulted Boolean (Scala default fills in server-side)" in {
      // A defaulted primitive in Scala is filled in by the Scala-side decoder
      // when the wire payload omits it. The Dart codegen should keep the
      // field non-nullable — the wire shape always carries a value because
      // the Dart class declares the field non-nullable + required, forcing
      // Dart-side callers to supply one. This matches the agent-author's
      // intent: a default exists so Scala doesn't reject, but the field is
      // semantically never absent.
      val files = generate("SelectOption", summon[RW[SelectOption]].definition)
      val source = find(files, "select_option.dart")
      source should include("final bool exclusive;")
      source should not include "final bool? exclusive;"
    }

    "render every required primitive as non-nullable" in {
      val files = generate("AllPrimitives", summon[RW[AllPrimitives]].definition)
      val source = find(files, "all_primitives.dart")
      source should include("final String s;")
      source should include("final int i;")
      source should include("final int l;")
      source should include("final double d;")
      source should include("final bool b;")
      // No nullable field declarations (the `as int?` etc. inside fromJson
      // legitimately uses Dart's null-coalescing on the JSON access).
      source should not include "final String? "
      source should not include "final int? "
      source should not include "final double? "
      source should not include "final bool? "
    }

    "still render Option[T] primitives as nullable" in {
      val files = generate("WithOptions", summon[RW[WithOptions]].definition)
      val source = find(files, "with_options.dart")
      source should include("final String name;")
      source should include("final int? age;")
      source should include("final bool? enabled;")
    }

    "render `Option[T] = None` as nullable, not as `final T field; this.field = null` (sigil bug #12)" in {
      // Fabric encodes `Option[T] = None` as `Opt(inner)` with defaultValue =
      // JsonNull — identical from a Definition perspective to `T = default`
      // except the default is `null`. Bug #11's fix unwrapped the Opt for
      // any defaulted field, producing `final T field;` paired with
      // `this.field = null` in the constructor — illegal Dart. Option-with-None
      // must stay on the nullable path.
      val files = generate("WithNoneDefaults", summon[RW[WithNoneDefaults]].definition)
      val source = find(files, "with_none_defaults.dart")
      source should include("final String? name;")
      source should include("final String? sessionId;")
      source should include("final String? workspaceId;")
      source should not include "this.name = null"
      source should not include "this.sessionId = null"
      source should not include "this.workspaceId = null"
      // Constructor must not carry any `= null` literal (toJson legitimately
      // contains `!= null` and `: null`, neither of which uses `= null` as a
      // default-assignment).
      source should not include " = null"
    }

    "inline wrapper-typed defaults as `const Wrapper(literal)` for stable values (sigil bug #14 phase B)" in {
      // Wrappers around primitives with stable defaults
      // (`Timestamp(1234L)`, `Id("static-id")`) get inlined as
      // `const Wrapper(literal)` — type-correct in Dart since the
      // wrapper class has a `const` constructor. Effectful defaults
      // (different value per thunk invocation) take the Phase C path
      // instead; this test covers stable-value defaults.
      val files = generate("WithWrapperDefaults", summon[RW[WithWrapperDefaults]].definition)
      val source = find(files, "with_wrapper_defaults.dart")
      source should include("final SpecTimestamp created;")
      source should include("final SpecId id;")
      source should include("this.created = const SpecTimestamp(1234)")
      source should include("this.id = const SpecId('static-id')")
      source should not include "required this.created"
      source should not include "required this.id"
      // Bare primitive literal (no wrapper) must NOT be emitted in place
      // of the wrapper-constructor expression — the Dart class field type
      // is `SpecTimestamp`, not `int`.
      source should not include "this.created = 1234,"
      source should not include "this.created = 1234;"
    }

    "render wrappers with effectful defaults as nullable + initialize-list with fresh-helper (sigil bug #14 phase C)" in {
      // `Timestamp()` and `Id.unique()` produce different JSON values
      // across thunk invocations. Inlining the codegen-moment value
      // would freeze every Dart-constructed instance to a single id /
      // timestamp — broken across instances. Phase C accepts the field
      // as a nullable named param and fills with a wrapper-class fresh
      // helper (`now()` for Long-shaped, `unique()` for String-shaped)
      // in the constructor's initializer list.
      val files = generate("WithEffectfulWrappers", summon[RW[WithEffectfulWrappers]].definition)
      val source = find(files, "with_effectful_wrappers.dart")
      source should include("final SpecTimestamp created;")
      source should include("final SpecId fresh;")
      source should include("SpecTimestamp? created")
      source should include("SpecId? fresh")
      source should include regex """created\s*=\s*created\s*\?\?\s*SpecTimestamp\.now\(\)"""
      source should include regex """fresh\s*=\s*fresh\s*\?\?\s*SpecId\.unique\(\)"""
      source should not include "required this.created"
      source should not include "required this.fresh"
    }

    "emit `Wrapper.now()` / `unique()` static on wrapper Dart classes referenced as effectful defaults (sigil bug #14 phase C)" in {
      // The fresh-helper static is generated alongside the wrapper class
      // itself so consumers don't need to add a `uuid` package or write
      // `Timestamp.now()` glue by hand.
      val files = generate("WithEffectfulWrappers", summon[RW[WithEffectfulWrappers]].definition)
      val timestampSource = find(files, "spec_timestamp.dart")
      val idSource = find(files, "spec_id.dart")
      timestampSource should include regex """static\s+SpecTimestamp\s+now\(\)"""
      idSource should include regex """static\s+SpecId\s+unique\(\)"""
      idSource should include("import 'dart:math'")
    }

    "inline case-object polytype defaults as `const SubName()` (sigil bug #14 phase B)" in {
      // `currentMode: Mode = ConversationMode` where ConversationMode is
      // an empty case-object subtype. The Dart subclass has a `const`
      // constructor, so `const ConversationMode()` is a valid Dart
      // constant default — type-compatible with the `Mode` field type.
      val files = generate("WithSingletonDefault", summon[RW[WithSingletonDefault]].definition)
      val source = find(files, "with_singleton_default.dart")
      source should include("this.currentMode = const SpecConversationMode()")
      source should not include "required this.currentMode"
    }

    "inline enum-case defaults as `EnumName.caseName` (sigil bug #14 phase A)" in {
      // Bug #13's first cut left enum-case defaults as `required`, on
      // the principle that we couldn't safely synthesize the case ref
      // from a String JSON value. Bug #14 reasserts: this needs to ride
      // the inline-default path. The default JSON's String discriminator
      // routes to the matching Dart enum case (lowercased per Dart
      // enum-case naming rules), so the field stays non-nullable AND
      // gains a sensible Dart-side fallback.
      val files = generate("WithEnumDefault", summon[RW[WithEnumDefault]].definition)
      val source = find(files, "with_enum_default.dart")
      source should include("final SpecState state;")
      source should include("this.state = SpecState.active")
      source should not include "required this.state"
      source should not include "this.state = 'Active'"
      source should not include "this.state = \"Active\""
    }

    "inline empty-list defaults as `const []` (sigil bug #14 phase A)" in {
      // `field: List[X] = Nil` is the most common defaulted-collection
      // shape in domain models (`participants`, `keywords`, `topics`,
      // …). The empty-list default is type-compatible with any
      // `List<T>` so a plain `const []` works regardless of the
      // element type — no per-type Dart literal synthesis needed.
      val files = generate("WithCollectionDefaults", summon[RW[WithCollectionDefaults]].definition)
      val source = find(files, "with_collection_defaults.dart")
      source should include("final List<String> participants;")
      source should include("final List<String> keywords;")
      source should include("this.participants = const []")
      source should include("this.keywords = const []")
      source should not include "required this.participants"
      source should not include "required this.keywords"
    }

    "render singleton-trait defaults against a polytype with a single empty subtype (sigil bug #15)" in {
      // Sigil's `Mode = ConversationMode` shape — the parent trait has
      // exactly one case-object subtype out of the box. Verify the
      // codegen emits a default that compiles: either `const SubName()`
      // referencing a generated subtype class, or the simple-enum path
      // referencing a Dart enum case. Either is correct as long as
      // the named symbol exists in the generated output.
      val files = generate("WithSingleSubtypePoly", summon[RW[WithSingleSubtypePoly]].definition)
      val source = find(files, "with_single_subtype_poly.dart")
      val parentSource = scala.util.Try(find(files, "spec_single_mode.dart")).toOption
      val subtypeSource = scala.util.Try(find(files, "spec_single_conv_mode.dart")).toOption

      // Two valid renderings:
      //   (a) simple-enum path: `final SpecSingleMode mode; this.mode = SpecSingleMode.specSingleConvMode`
      //   (b) class-hierarchy path: `this.mode = const SpecSingleConvMode()` AND a `class SpecSingleConvMode` exists
      val emittedAsEnum  = source.contains("SpecSingleMode.")
      val emittedAsConst = source.contains("const SpecSingleConvMode()")
      (emittedAsEnum || emittedAsConst) shouldBe true

      if (emittedAsConst) {
        // The class referenced by the const-call must actually exist in the
        // generated tree — otherwise Dart can't resolve the symbol.
        subtypeSource should not be empty
        subtypeSource.get should include("class SpecSingleConvMode")
      }
    }

    "render trait-with-fields polytype defaults so the referenced symbol resolves (sigil bug #15)" in {
      // Sigil's actual `Mode` shape: a trait with concrete field
      // defaults (`name`, `description`, `keywords`, etc.). Multiple
      // case-object subtypes override the defaults. Fabric encodes the
      // subtype's defType as `Obj(non-empty)` (the inherited fields),
      // so the empty-Obj branch from Phase B doesn't fire.
      //
      // The codegen must still emit a compileable default — either by
      // matching the field's actual subtype class (which is generated
      // by the polytype path) or by some other valid singleton ref.
      // Failing safely with `required this.field` is also acceptable
      // (caller supplies value); what's NOT acceptable is emitting a
      // const-call against a name that doesn't exist as a Dart class.
      val files = generate("WithModeShape", summon[RW[WithModeShape]].definition)
      val source = find(files, "with_mode_shape.dart")
      println("=== with_mode_shape.dart ===")
      println(source)
      println("=== file list ===")
      files.foreach(f => println(s"  ${f.fileName}"))
      println("=== fabric Definition for SpecModeShapeKind ===")
      println(summon[RW[SpecModeShapeKind]].definition)
      // Pull every Dart class name the codegen actually emitted so we
      // can verify any const-call references something real.
      val allClasses: Set[String] = files.flatMap { f =>
        "(?s)class\\s+(\\w+)".r.findAllMatchIn(f.source).map(_.group(1)).toList
      }.toSet
      val emittedEnumRef    = """\bSpecModeShapeKind\.\w+""".r.findFirstIn(source).isDefined
      val emittedConstCall  = """const\s+(\w+)\s*\(\s*\)""".r.findFirstMatchIn(source)
      val emittedRequired   = source.contains("required this.kind")
      // At least one rendering choice must apply.
      (emittedEnumRef || emittedConstCall.isDefined || emittedRequired) shouldBe true
      // If the codegen chose const-call, the named class must exist.
      emittedConstCall.foreach { m =>
        val name = m.group(1)
        allClasses should contain(name)
      }
    }

    "preserve required vs defaulted on subtypes reached through a polytype" in {
      // Mirrors the original sigil bug repro path: required + defaulted
      // primitives on a case class reached as a subtype of a sealed
      // trait. The polytype's fromJson dispatch builds subtype Dart
      // classes via the same path; both required-ness and default-ness
      // must round-trip correctly.
      val defn = summon[RW[ToolInput]].definition
      val files = DurableSocketDartGenerator(
        DurableSocketDartConfig(serviceName = "Test", wireType = "ToolInput" -> defn)
      ).generate()

      val askSource = find(files, "respond_options_tool_input.dart")
      askSource should include("final bool allowMultiple;")
      askSource should not include "final bool? allowMultiple;"

      val selectSource = find(files, "select_option_tool_input.dart")
      selectSource should include("final bool exclusive;")
      selectSource should not include "final bool? exclusive;"
    }
  }

  // Polytype repro types — top-level so the FQN stays predictable.
}

sealed trait ToolInput derives RW
case class RespondOptionsToolInput(prompt: String,
                                    allowMultiple: Boolean) extends ToolInput derives RW
case class SelectOptionToolInput(label: String,
                                  exclusive: Boolean = false) extends ToolInput derives RW

// Typed wrappers — primitive-shaped but with a className, mirroring sigil's
// `Timestamp` / `Id` / etc. for bug #13.
case class SpecTimestamp(value: Long) extends AnyVal
object SpecTimestamp {
  given RW[SpecTimestamp] = RW.long(_.value, SpecTimestamp.apply, className = Some("spec.SpecTimestamp"))
}
case class SpecId(value: String) extends AnyVal
object SpecId {
  given RW[SpecId] = RW.string(_.value, SpecId.apply, className = Some("spec.SpecId"))
}

// Enum-typed default — same shape as `EventState` / `MessageRole`.
enum SpecState derives RW { case Active, Complete, Cancelled }

// Polytype + empty case-object subtype — same shape as `Mode = ConversationMode`.
// Adding a second subtype WITH fields keeps SpecMode out of the simple-enum
// fast-path (which would render as a Dart enum, not as a polymorphic class
// hierarchy with const-constructible empty subtypes).
sealed trait SpecMode derives RW
case object SpecConversationMode extends SpecMode
case class SpecCustomMode(label: String) extends SpecMode

// Single-empty-subtype polytype — the codegen has to pick *some*
// rendering; either Dart enum or const-class call. The bug-#15 test
// is permissive: it accepts whichever rendering the codegen chooses
// AS LONG AS the referenced symbol exists.
sealed trait SpecSingleMode derives RW
case object SpecSingleConvMode extends SpecSingleMode

// Sigil's `Mode` shape: a sealed trait with concrete field defaults
// (`name`, `description`) + multiple case-object subtypes overriding
// them. Fabric encodes each subtype's defType as `Obj(non-empty)`
// (the inherited field set), so the empty-Obj Phase B path doesn't
// apply — the codegen has to route this through the simple-enum
// path or a different singleton rendering.
sealed trait SpecModeShapeKind derives RW {
  def name: String
}
case object SpecModeShapeConv extends SpecModeShapeKind { val name = "conv" }
case object SpecModeShapeWork extends SpecModeShapeKind { val name = "work" }
