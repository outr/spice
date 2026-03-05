package spice.api

import scala.quoted.*
import spice.net.URL

object WsClientMacro {
  def connect[T: Type](
    wsUrl: Expr[URL],
    handler: Expr[T]
  )(using Quotes): Expr[rapid.Task[spice.http.WebSocket]] = {
    import quotes.reflect.*

    val tpe = TypeRepr.of[T]
    val tpeSym = tpe.typeSymbol

    if (!tpeSym.flags.is(Flags.Trait)) {
      report.errorAndAbort(s"WsClient.connect requires a trait, but got ${tpeSym.name}")
    }

    val abstractMethods = tpe.baseClasses.flatMap(_.declarations.filter { s =>
      s.isDefDef && s.flags.is(Flags.Deferred)
    }).distinctBy(_.name)

    if (abstractMethods.isEmpty) {
      report.errorAndAbort(s"Trait ${tpeSym.name} has no abstract methods to handle")
    }

    // Collect method info for dispatch
    val methodInfos = abstractMethods.map { method =>
      val methodName = method.name
      val methodType = tpe.memberType(method)
      val (paramClauses, returnType) = unwrapMethodType(methodType)

      // Validate return type is Task[Unit]
      val taskSym = TypeRepr.of[rapid.Task[?]].typeSymbol
      returnType match {
        case AppliedType(base, List(inner)) if base.typeSymbol == taskSym =>
          if (!(inner =:= TypeRepr.of[Unit])) {
            report.errorAndAbort(s"WsClient handler method '$methodName' must return Task[Unit]")
          }
        case _ =>
          report.errorAndAbort(s"Method '$methodName' must return Task[Unit], but returns ${returnType.show}")
      }

      val allParams = paramClauses.flatten
      (method, allParams)
    }

    // Generate the dispatch function as a lambda: (String, Json) => Unit
    val dispatchFn = buildDispatch[T](handler, methodInfos)

    '{
      WsClientRuntime.connect($wsUrl, $dispatchFn)
    }
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

  private def buildDispatch[T: Type](using Quotes)(
    handler: Expr[T],
    methods: List[(quotes.reflect.Symbol, List[(String, quotes.reflect.TypeRepr)])]
  ): Expr[(String, fabric.Json) => Unit] = {
    import quotes.reflect.*

    val jsonType = TypeRepr.of[fabric.Json]
    val stringType = TypeRepr.of[String]

    // Build match cases for each method
    val cases: List[Expr[(String, fabric.Json) => Unit] => Expr[Unit]] = methods.map { case (method, params) =>
      val methodName = method.name
      (fn: Expr[(String, fabric.Json) => Unit]) => {
        // Build the handler call with params extracted from json
        val args: List[Expr[?]] = params.map { case (pName, pType) =>
          pType.asType match {
            case '[p] =>
              val pRW = Expr.summon[fabric.rw.RW[p]].getOrElse(
                report.errorAndAbort(s"No RW for ${pType.show}")
              )
              val nameExpr = Expr(pName)
              // This will be used inside the lambda
              '{ ??? : p } // placeholder - we'll build this differently
          }
        }
        '{ () }
      }
    }

    // Use a simpler approach: build the full dispatch function body at the tree level
    val lambdaType = MethodType(List("method", "args"))(
      _ => List(stringType, jsonType),
      _ => TypeRepr.of[Unit]
    )

    Lambda(Symbol.spliceOwner, lambdaType, { (_, argsList) =>
      val methodRef = argsList(0).asExprOf[String]
      val argsRef = argsList(1).asExprOf[fabric.Json]

      // Build if-else chain for dispatch
      val dispatchExpr = methods.foldRight('{ () }: Expr[Unit]) { case ((method, params), elseExpr) =>
        val methodName = Expr(method.name)

        // Build the call: handler.methodName(args("p1").as[P1], ...)
        val callArgs: List[Term] = params.map { case (pName, pType) =>
          pType.asType match {
            case '[p] =>
              val pRW = Expr.summon[fabric.rw.RW[p]].getOrElse(
                report.errorAndAbort(s"No RW for ${pType.show}")
              )
              val nameExpr = Expr(pName)
              '{ fabric.rw.Asable($argsRef($nameExpr)).as[p](using $pRW) }.asTerm
          }
        }

        val methodSym = TypeRepr.of[T].baseClasses.flatMap(_.declarations)
          .find(s => s.name == method.name && s.isDefDef).get
        val callExpr = if (callArgs.isEmpty) {
          Select(handler.asTerm, methodSym).appliedToNone.asExprOf[rapid.Task[Unit]]
        } else {
          Select(handler.asTerm, methodSym).appliedToArgs(callArgs).asExprOf[rapid.Task[Unit]]
        }

        '{ if ($methodRef == $methodName) { $callExpr.start(); () } else $elseExpr }
      }

      dispatchExpr.asTerm
    }).asExprOf[(String, fabric.Json) => Unit]
  }
}
