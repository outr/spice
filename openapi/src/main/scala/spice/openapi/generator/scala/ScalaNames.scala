package spice.openapi.generator.scala

/** Scala-class -> generated-Scala-name rules for [[OpenAPIScalaGenerator]].
  *
  * Mirrors `KotlinNames`: a className splits into a leading lowercase-first "package" run and the
  * remaining uppercase-first "class chain". The generated type name is the class chain concatenated
  * (so `Watchable.Movie` -> `WatchableMovie`, keeping distinct sum types' same-named cases apart), and
  * the wire discriminator is the chain dot-joined to match Fabric's `Definition.simpleClassName`.
  *
  * Also carries the reserved-word escaping the other generators do not need. A third-party schema is
  * free to name a property `type` or `class`, which are keywords here; those become backticked so the
  * emitted source compiles while the wire name is preserved.
  */
object ScalaNames {
  /** Scala 3 keywords that a schema property is realistically going to collide with. Backticking is
    * always safe, so this errs toward including a word rather than emitting something that will not
    * compile. */
  private val Reserved: Set[String] = Set(
    "abstract", "case", "catch", "class", "def", "do", "else", "enum", "export", "extends", "false",
    "final", "finally", "for", "given", "if", "implicit", "import", "lazy", "match", "new", "null",
    "object", "override", "package", "private", "protected", "return", "sealed", "super", "then",
    "this", "throw", "trait", "true", "try", "type", "val", "var", "while", "with", "yield"
  )

  def stripTypeArgs(cn: String): String = {
    val idx = cn.indexOf('[')
    if (idx == -1) cn else cn.substring(0, idx)
  }

  def splitClassName(cn: String): (List[String], List[String]) = {
    val cleaned = stripTypeArgs(cn).replace("$", ".")
    val parts = cleaned.split('.').toList.filter(p => p.nonEmpty && p != "anon" && !p.forall(_.isDigit))
    if (parts.lengthIs <= 1) (List.empty, parts)
    else parts.span(p => p.charAt(0).isLower && !p.contains("-") && !p.contains("_"))
  }

  private def pascal(s: String): String =
    if (s.isEmpty) s
    else s.split("[-_]+").filter(_.nonEmpty).map(p => p.head.toUpper +: p.tail).mkString

  /** Type name: the class chain concatenated, PascalCase'd. */
  def className(cn: String): String = {
    val (_, chain) = splitClassName(cn)
    val raw =
      if (chain.nonEmpty) chain.mkString
      else {
        val cleaned = stripTypeArgs(cn).replace("$", ".")
        cleaned.split('.').toList.filter(_.nonEmpty).lastOption.getOrElse(cn.replace(" ", "").replace(".", ""))
      }
    pascal(raw)
  }

  /** Wire discriminator value: the class chain dot-joined (Fabric's simpleClassName). */
  def wireDiscriminator(cn: String): String = {
    val (_, chain) = splitClassName(cn)
    if (chain.nonEmpty) chain.mkString(".") else cn.replace(" ", "")
  }

  /** A property name safe to use as a case-class parameter. Backticked when it is a keyword; the wire
    * name is unchanged, because fabric derives the JSON key from the parameter name. */
  def propName(wire: String): String = if (Reserved.contains(wire)) s"`$wire`" else wire

  /** An enum case name. Schema enum values are wire strings ("tv_show", "buy"), which are neither
    * valid nor idiomatic as case names, so they are PascalCase'd and the original is kept as the
    * enum's wire value. */
  def enumCaseName(value: String): String = {
    val cleaned = pascal(value.map(ch => if (ch.isLetterOrDigit) ch else '_'))
    if (cleaned.isEmpty) "Unknown"
    else if (cleaned.head.isDigit) s"_$cleaned"
    else if (Reserved.contains(cleaned)) s"`$cleaned`"
    else cleaned
  }
}
