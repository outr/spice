package spice.api.server

import scala.quoted.*

object DurableEventIntrospectMacro {
  def introspect[T: Type](using Quotes): Expr[List[DurableEventDescriptor]] = {
    import quotes.reflect.*

    val tpe = TypeRepr.of[T]
    val tpeSym = tpe.typeSymbol

    // Must be a sealed trait or sealed abstract class
    if (!tpeSym.flags.is(Flags.Sealed)) {
      report.errorAndAbort(s"DurableEventIntrospect.events requires a sealed trait, but got ${tpeSym.name}")
    }

    // Get all case class children of the sealed trait
    val children = tpeSym.children.filter(_.flags.is(Flags.Case))

    if (children.isEmpty) {
      report.errorAndAbort(s"Sealed trait ${tpeSym.name} has no case class variants")
    }

    val descriptorExprs = children.map { child =>
      val className = child.name
      val snakeName = toSnakeCase(className)

      // Get constructor parameters via the constructor's method type
      val ctorType = child.primaryConstructor.tree.asInstanceOf[DefDef].termParamss.flatMap(_.params)
      val fields = ctorType.map { valDef =>
        val paramName = valDef.name
        val paramType = valDef.tpt.tpe
        val (typeName, optional) = normalizeType(paramType)
        '{ DurableFieldDescriptor(${ Expr(paramName) }, ${ Expr(typeName) }, ${ Expr(optional) }) }
      }

      '{ DurableEventDescriptor(${ Expr(snakeName) }, ${ Expr.ofList(fields) }) }
    }

    Expr.ofList(descriptorExprs)
  }

  private def toSnakeCase(name: String): String = {
    val result = new StringBuilder
    for (i <- name.indices) {
      val c = name(i)
      if (c.isUpper && i > 0) {
        result.append('_')
      }
      result.append(c.toLower)
    }
    result.toString
  }

  private def normalizeType(using Quotes)(tpe: quotes.reflect.TypeRepr): (String, Boolean) = {
    import quotes.reflect.*
    tpe.dealias match {
      case AppliedType(base, List(inner)) if base.typeSymbol == TypeRepr.of[Option[?]].typeSymbol =>
        val (innerName, _) = normalizeType(inner)
        (innerName, true)
      case AppliedType(base, List(inner)) if base.typeSymbol == TypeRepr.of[List[?]].typeSymbol =>
        val (innerName, _) = normalizeType(inner)
        (s"List[$innerName]", false)
      case t if t =:= TypeRepr.of[String] => ("String", false)
      case t if t =:= TypeRepr.of[Int] => ("Int", false)
      case t if t =:= TypeRepr.of[Long] => ("Long", false)
      case t if t =:= TypeRepr.of[Boolean] => ("Boolean", false)
      case t if t =:= TypeRepr.of[Double] => ("Double", false)
      case other =>
        report.errorAndAbort(s"Unsupported type for DurableEvent Dart generation: ${other.show}")
    }
  }
}
