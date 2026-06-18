package spice.openapi.generator.kotlin

/** Scala-class -> Kotlin-name rules for [[OpenAPIKotlinGenerator]].
  *
  * Mirrors `DartNames`: a className splits into a leading lowercase-first "package" run and the
  * remaining uppercase-first "class chain". The Kotlin class name is the class chain concatenated
  * (so `Watchable.Movie` -> `WatchableMovie`, keeping distinct sum types' same-named cases apart),
  * and the wire discriminator is the chain dot-joined to match Fabric's `Definition.simpleClassName`
  * (`Watchable.Movie`). Unlike Dart we emit one file per class into a single flat package, so there
  * are no cross-model imports to compute.
  */
object KotlinNames {
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

  private def sanitize(s: String): String =
    if (s.isEmpty) s
    else s.split("[-_]+").filter(_.nonEmpty).map(p => p.head.toUpper +: p.tail).mkString

  /** Kotlin class name: the class chain concatenated, PascalCase'd. */
  def className(cn: String): String = {
    val (_, chain) = splitClassName(cn)
    val raw =
      if (chain.nonEmpty) chain.mkString
      else {
        val cleaned = stripTypeArgs(cn).replace("$", ".")
        cleaned.split('.').toList.filter(_.nonEmpty).lastOption.getOrElse(cn.replace(" ", "").replace(".", ""))
      }
    sanitize(raw)
  }

  /** Wire discriminator value: the class chain dot-joined (Fabric's simpleClassName). */
  def wireDiscriminator(cn: String): String = {
    val (_, chain) = splitClassName(cn)
    if (chain.nonEmpty) chain.mkString(".") else cn.replace(" ", "")
  }
}
