package spice.openapi.generator.dart

import fabric.define.Definition

/** Single source of truth for the Scala-class → Dart-name conversion rules used by every
  * Dart code generator in this module.
  *
  * Both `OpenAPIDartGenerator` and `DurableSocketDartGenerator` delegate to these helpers
  * so the rules cannot drift between the two generators — there is only one implementation.
  *
  * Naming model:
  *
  *   - A Scala class name is split into a leading run of lowercase-first segments (the
  *     "package") and the remaining uppercase-first segments (the "class chain").
  *   - The Dart class name is the class chain concatenated. This naturally qualifies
  *     nested cases (`ResponseContent.Text` → `ResponseContentText`) so distinct polymorphic
  *     parents declaring same-named cases land on distinct Dart classes.
  *   - The wire discriminator stays the leaf simple class name to match Fabric's
  *     `Product.productPrefix` write side.
  *   - The Dart file path mirrors the package structure under `lib/model`.
  *
  * Type-parameter syntax (`Id[User]`) is stripped before any name is computed.
  * Scala 3 anonymous-class noise (`$anon`, `.anon.NN`, digit-only segments) is filtered
  * out so that anon enum cases like `Status$$anon$1` don't pollute Dart names. */
object DartNames {

  /** Strip type arguments from a className: `"Id[User]"` → `"Id"`. */
  def stripTypeArgs(cn: String): String = {
    val idx = cn.indexOf('[')
    if (idx == -1) cn else cn.substring(0, idx)
  }

  /** Split a cleaned className into (packageSegments, classChainSegments).
    *
    *   "com.example.foo.Bar"                  → (List(com, example, foo), List(Bar))
    *   "spec.OpenAPIHttpServerSpec.Auth"      → (List(spec), List(OpenAPIHttpServerSpec, Auth))
    *   "sigil.tool.model.ResponseContent.Text" → (List(sigil, tool, model), List(ResponseContent, Text))
    *
    * `$` is normalized to `.` first, and noise segments (empty, "anon", digit-only) are
    * dropped so Scala 3 anonymous-case-object names don't leak into Dart identifiers.
    *
    * Hyphenated / underscored tokens — e.g. `"workflow-builder"` written by a polymorphic
    * RW whose discriminator value is a kebab-case identifier rather than a Scala class name
    * — count as class-chain elements, not package segments. Treating them as packages would
    * leak the hyphen into the file system path and produce invalid Dart class names. */
  def splitClassName(cn: String): (List[String], List[String]) = {
    val cleaned = stripTypeArgs(cn).replace("$", ".")
    val parts = cleaned.split('.').toList.filter(p => p.nonEmpty && p != "anon" && !p.forall(_.isDigit))
    // A single segment never represents a package — it's always a leaf type
    // name. Routing a single-segment lowercase name into the package portion
    // would produce a `lib/model/<name>/<name>.dart` layout the polymorphic
    // base's `const` field references can't resolve to the actual class.
    if (parts.lengthIs <= 1) (List.empty, parts)
    else parts.span(p => p.charAt(0).isLower && !p.contains("-") && !p.contains("_"))
  }

  /** Coerce a raw discriminator string into a syntactically-valid Dart class name.
    * Hyphens and underscores are treated as word separators; each separated word is
    * upper-cased on its first letter and concatenated. Single-word inputs (no
    * separator) are still capitalised — a bare `"coding"` becomes `"Coding"` so the
    * generated polymorphic base's `static const Mode coding = Coding();` field
    * parses cleanly in Dart's const context. Already-capitalised inputs (`"Coding"`,
    * `"WorkflowBuilder"`) pass through unchanged. */
  private def sanitizeDartIdentifier(s: String): String =
    if (s.isEmpty) s
    else s.split("[-_]+").filter(_.nonEmpty).map(p => p.head.toUpper +: p.tail).mkString

  /** Concatenate the className's class chain into a single Dart class name.
    *
    *   "sigil.tool.model.ResponseContent.Text"   → "ResponseContentText"
    *   "sigil.conversation.ContextFrame.Text"    → "ContextFrameText"
    *   "lightdb.id.Id"                           → "Id"
    *   "workflow-builder"                        → "WorkflowBuilder"
    *
    * Falls back to the leaf segment (or the whole input with separators stripped) when no
    * class chain can be identified. Hyphenated / underscored inputs are PascalCase'd so
    * the result is a syntactically-valid Dart identifier. */
  def dartClassName(cn: String): String = {
    val (_, classChain) = splitClassName(cn)
    val raw =
      if (classChain.nonEmpty) classChain.mkString
      else {
        val cleaned = stripTypeArgs(cn).replace("$", ".")
        val parts = cleaned.split('.').toList.filter(_.nonEmpty)
        parts.lastOption.getOrElse(cn.replace(" ", "").replace(".", ""))
      }
    sanitizeDartIdentifier(raw)
  }

  /** Dart class name for a polymorphic subtype.
    *
    * Uses the className's class chain when available (so `ResponseContent.Text` produces
    * `ResponseContentText`). Falls back to `parentDartName + key` for anonymous Scala 3
    * enum cases (whose className looks like `Status$$anon$1` and yields no useful chain). */
  def dartSubtypeName(key: String, defn: Definition, parentDartName: Option[String] = None): String = {
    val derived: String = defn.className match {
      case Some(cn) if !cn.contains("anon") => dartClassName(cn)
      case _ => ""
    }
    if (derived.nonEmpty && !derived.forall(_.isDigit)) derived
    else parentDartName.map(p => s"$p${sanitizeDartIdentifier(key)}").getOrElse(sanitizeDartIdentifier(key))
  }

  /** Wire discriminator value for a className — matches Fabric's `Product.productPrefix`
    * (the simple leaf class name).
    *
    *   "sigil.tool.model.ResponseContent.Text" → "Text"
    *   "spec.OpenAPIHttpServerSpec.Auth"        → "Auth" */
  def wireDiscriminator(cn: String): String = {
    val (_, classChain) = splitClassName(cn)
    classChain.lastOption.getOrElse(cn.replace(" ", ""))
  }

  /** Package-derived path segment (no leading slash).
    *
    *   "com.example.foo.Bar"                     → "com/example/foo"
    *   "scalagentic.conversation.event.Deleted"  → "scalagentic/conversation/event"
    *   "spec.OpenAPIHttpServerSpec.Auth"         → "spec"
    *   "Bar" (no package)                        → "" */
  def packagePath(cn: String): String = {
    val (pkg, _) = splitClassName(cn)
    pkg.mkString("/")
  }

  /** Directory path for a class under the given root (e.g. "lib/model").
    *
    *   modelPathFor("com.example.foo.Bar")           → "lib/model/com/example/foo"
    *   modelPathFor("spec.OpenAPIHttpServerSpec.Auth") → "lib/model/spec"
    *   modelPathFor("Bar")                           → "lib/model" */
  def modelPathFor(cn: String, root: String = "lib/model"): String = {
    val pkg = packagePath(cn)
    if (pkg.isEmpty) root else s"$root/$pkg"
  }

  /** Compute a relative path from one directory to a file in another directory. */
  def relativeImport(fromDir: String, toDir: String, fileName: String): String = {
    if (fromDir == toDir) fileName
    else {
      val fromParts = fromDir.split("/").toList.filter(_.nonEmpty)
      val toParts = toDir.split("/").toList.filter(_.nonEmpty)
      val commonLen = fromParts.zip(toParts).takeWhile { case (a, b) => a == b }.length
      val ups = "../" * (fromParts.length - commonLen)
      val downs = toParts.drop(commonLen).mkString("/")
      if (downs.nonEmpty) s"$ups$downs/$fileName" else s"$ups$fileName"
    }
  }

  /** Convert a Dart class name to a snake_case file basename:
    * "OpenAPIHttpServerSpecAuth" → "open_a_p_i_http_server_spec_auth". */
  def snakeCaseFile(dartName: String): String = {
    if (dartName.isEmpty) dartName
    else {
      val pre = dartName.charAt(0).toLower
      val suffix = "\\p{Lu}".r.replaceAllIn(dartName.substring(1), m => s"_${m.group(0).toLowerCase}")
      s"$pre$suffix".replace(" ", "")
    }
  }
}
