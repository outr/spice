package spice.api.server

import scala.quoted.*

object WsIntrospectMacro {
  def introspect[T: Type](using Quotes): Expr[List[WsMethodDescriptor]] = {
    import quotes.reflect.*

    val tpe = TypeRepr.of[T]
    val tpeSym = tpe.typeSymbol

    if (!tpeSym.flags.is(Flags.Trait)) {
      report.errorAndAbort(s"WsIntrospect.methods requires a trait, but got ${tpeSym.name}")
    }

    val abstractMethods = tpe.baseClasses.flatMap(_.declarations.filter { s =>
      s.isDefDef && s.flags.is(Flags.Deferred)
    }).distinctBy(_.name)

    if (abstractMethods.isEmpty) {
      report.errorAndAbort(s"Trait ${tpeSym.name} has no abstract methods to introspect")
    }

    val methodExprs = abstractMethods.map { method =>
      val methodName = method.name
      val methodType = tpe.memberType(method)
      val (paramClauses, returnType) = unwrapMethodType(methodType)

      // Validate return type is Task[Unit]
      val taskSym = TypeRepr.of[rapid.Task[?]].typeSymbol
      returnType match {
        case AppliedType(base, List(inner)) if base.typeSymbol == taskSym =>
          if (!(inner =:= TypeRepr.of[Unit])) {
            report.errorAndAbort(s"WsIntrospect method '$methodName' must return Task[Unit]")
          }
        case _ =>
          report.errorAndAbort(s"Method '$methodName' must return Task[Unit], but returns ${returnType.show}")
      }

      val allParams = paramClauses.flatten
      val paramExprs = allParams.map { case (pName, pType) =>
        val (typeName, optional) = normalizeType(pType)
        '{ WsParamDescriptor(${ Expr(pName) }, ${ Expr(typeName) }, ${ Expr(optional) }) }
      }

      '{ WsMethodDescriptor(${ Expr(methodName) }, ${ Expr.ofList(paramExprs) }) }
    }

    Expr.ofList(methodExprs)
  }

  private def unwrapMethodType(using Quotes)(tpe: quotes.reflect.TypeRepr): (List[List[(String, quotes.reflect.TypeRepr)]], quotes.reflect.TypeRepr) = {
    import quotes.reflect.*
    tpe match {
      case MethodType(paramNames, paramTypes, resType) =>
        val (rest, ret) = unwrapMethodType(resType)
        (paramNames.zip(paramTypes) :: rest, ret)
      case other => (Nil, other)
    }
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
        report.errorAndAbort(s"Unsupported type for WS Dart generation: ${other.show}")
    }
  }
}
