package spice.api.server

import scala.quoted.*

object ApiIntrospectMacro {
  def introspect[T: Type](using Quotes): Expr[List[ApiMethodDescriptor]] = {
    import quotes.reflect.*

    val tpe = TypeRepr.of[T]
    val tpeSym = tpe.typeSymbol

    if (!tpeSym.flags.is(Flags.Trait)) {
      report.errorAndAbort(s"ApiIntrospect.methods requires a trait, but got ${tpeSym.name}")
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

      // Validate return type is Task[R]
      val taskSym = TypeRepr.of[rapid.Task[?]].typeSymbol
      val responseType = returnType match {
        case AppliedType(base, List(inner)) if base.typeSymbol == taskSym => inner
        case _ =>
          report.errorAndAbort(s"Method '$methodName' must return Task[R], but returns ${returnType.show}")
      }

      val allParams = paramClauses.flatten
      val httpMethod = if (allParams.isEmpty) "get" else "post"

      // Summon RW for response type
      val responseRWExpr = responseType.asType match {
        case '[r] =>
          Expr.summon[fabric.rw.RW[r]].getOrElse(
            report.errorAndAbort(s"No RW instance found for response type ${responseType.show} in method '$methodName'")
          )
      }

      if (allParams.isEmpty) {
        // GET — no params, no request RW
        '{
          ApiMethodDescriptor(
            name = ${ Expr(methodName) },
            httpMethod = ${ Expr(httpMethod) },
            params = Nil,
            requestRW = None,
            responseRW = $responseRWExpr
          )
        }
      } else if (allParams.size == 1 && isCaseClass(allParams.head._2)) {
        // Single case class param — use its RW directly
        val (_, pType) = allParams.head
        val requestRWExpr = pType.asType match {
          case '[p] =>
            Expr.summon[fabric.rw.RW[p]].getOrElse(
              report.errorAndAbort(s"No RW instance found for request type ${pType.show} in method '$methodName'")
            )
        }
        '{
          ApiMethodDescriptor(
            name = ${ Expr(methodName) },
            httpMethod = ${ Expr(httpMethod) },
            params = Nil,
            requestRW = Some($requestRWExpr),
            responseRW = $responseRWExpr
          )
        }
      } else {
        // Multiple params — provide individual param RWs
        val paramExprs = allParams.map { case (pName, pType) =>
          pType.asType match {
            case '[p] =>
              val pRW = Expr.summon[fabric.rw.RW[p]].getOrElse(
                report.errorAndAbort(s"No RW instance found for param type ${pType.show} in method '$methodName'")
              )
              '{ ApiParamDescriptor(${ Expr(pName) }, $pRW) }
          }
        }
        '{
          ApiMethodDescriptor(
            name = ${ Expr(methodName) },
            httpMethod = ${ Expr(httpMethod) },
            params = ${ Expr.ofList(paramExprs) },
            requestRW = None,
            responseRW = $responseRWExpr
          )
        }
      }
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

  private def isCaseClass(using Quotes)(tpe: quotes.reflect.TypeRepr): Boolean = {
    import quotes.reflect.*
    val sym = tpe.typeSymbol
    sym.flags.is(Flags.Case) && sym.isClassDef
  }
}
