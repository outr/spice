package spice.api.server

import scala.quoted.*
import spice.http.server.MutableHttpServer
import spice.net.URLPath

object ApiServerMacro {
  def mount[T: Type](
    impl: Expr[T],
    server: Expr[MutableHttpServer],
    basePath: Expr[URLPath]
  )(using Quotes): Expr[Unit] = {
    import quotes.reflect.*

    val tpe = TypeRepr.of[T]
    val tpeSym = tpe.typeSymbol

    if (!tpeSym.flags.is(Flags.Trait)) {
      report.errorAndAbort(s"ApiServer.mount requires a trait, but got ${tpeSym.name}")
    }

    val abstractMethods = tpe.baseClasses.flatMap(_.declarations.filter { s =>
      s.isDefDef && s.flags.is(Flags.Deferred)
    }).distinctBy(_.name)

    if (abstractMethods.isEmpty) {
      report.errorAndAbort(s"Trait ${tpeSym.name} has no abstract methods to mount")
    }

    val mountExprs: List[Expr[Unit]] = abstractMethods.map { method =>
      val methodName = method.name
      val methodType = tpe.memberType(method)
      val (paramClauses, returnType) = unwrapMethodType(methodType)

      val taskSym = TypeRepr.of[rapid.Task[?]].typeSymbol
      returnType match {
        case AppliedType(base, _) if base.typeSymbol == taskSym => // ok
        case _ =>
          report.errorAndAbort(s"Method '$methodName' must return Task[R], but returns ${returnType.show}")
      }

      val AppliedType(_, List(responseType)) = returnType: @unchecked
      val allParams = paramClauses.flatten

      generateHandler(impl, server, basePath, methodName, allParams, responseType)
    }

    Expr.block(mountExprs.init, mountExprs.last)
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

  private def generateHandler[T: Type](using Quotes)(
    impl: Expr[T],
    server: Expr[MutableHttpServer],
    basePath: Expr[URLPath],
    methodName: String,
    params: List[(String, quotes.reflect.TypeRepr)],
    responseType: quotes.reflect.TypeRepr
  ): Expr[Unit] = {
    import quotes.reflect.*

    responseType.asType match {
      case '[r] =>
        val resRW = Expr.summon[fabric.rw.RW[r]].getOrElse(
          report.errorAndAbort(s"No RW for ${responseType.show}")
        )
        val nameExpr = Expr(methodName)

        if (params.isEmpty) {
          val callExpr = makeImplCall0[T, r](impl, methodName)
          '{
            ApiServerRuntime.mountGet[r](
              $server, $basePath, $nameExpr,
              () => $callExpr
            )(using $resRW)
          }
        } else if (params.size == 1 && isCaseClass(params.head._2)) {
          val (_, pType) = params.head
          pType.asType match {
            case '[p] =>
              val pRW = Expr.summon[fabric.rw.RW[p]].getOrElse(
                report.errorAndAbort(s"No RW for ${pType.show}")
              )
              '{
                ApiServerRuntime.mountRestful[p, r](
                  $server, $basePath, $nameExpr,
                  (req: p) => ${ makeImplCall1[T, p, r](impl, methodName, 'req) }
                )(using $pRW, $resRW)
              }
          }
        } else {
          // Multi-param: build a Json => Task[R] function
          // We construct the lambda body that extracts each param from json
          val handlerFn = buildJsonHandler[T, r](impl, methodName, params)
          '{
            ApiServerRuntime.mountJson[r](
              $server, $basePath, $nameExpr,
              $handlerFn
            )(using $resRW)
          }
        }
    }
  }

  private def makeImplCall0[T: Type, R: Type](using Quotes)(
    impl: Expr[T],
    methodName: String
  ): Expr[rapid.Task[R]] = {
    import quotes.reflect.*
    val methodSym = TypeRepr.of[T].baseClasses.flatMap(_.declarations)
      .find(s => s.name == methodName && s.isDefDef).get
    Select(impl.asTerm, methodSym).appliedToNone.asExprOf[rapid.Task[R]]
  }

  private def makeImplCall1[T: Type, P: Type, R: Type](using Quotes)(
    impl: Expr[T],
    methodName: String,
    arg: Expr[P]
  ): Expr[rapid.Task[R]] = {
    import quotes.reflect.*
    val methodSym = TypeRepr.of[T].baseClasses.flatMap(_.declarations)
      .find(s => s.name == methodName && s.isDefDef).get
    Select(impl.asTerm, methodSym).appliedToArgs(List(arg.asTerm)).asExprOf[rapid.Task[R]]
  }

  private def buildJsonHandler[T: Type, R: Type](using Quotes)(
    impl: Expr[T],
    methodName: String,
    params: List[(String, quotes.reflect.TypeRepr)]
  ): Expr[fabric.Json => rapid.Task[R]] = {
    import quotes.reflect.*

    // Create a lambda: (json: Json) => { val p1 = json("p1").as[P1]; ... ; impl.method(p1, ...) }
    val jsonType = TypeRepr.of[fabric.Json]
    val taskRType = TypeRepr.of[rapid.Task[R]]
    val lambdaType = MethodType(List("json"))(_ => List(jsonType), _ => taskRType)

    Lambda(Symbol.spliceOwner, lambdaType, { (meth, argsList) =>
      val jsonRef = argsList.head.asExprOf[fabric.Json]

      val extractedArgs: List[Term] = params.map { case (pName, pType) =>
        pType.asType match {
          case '[p] =>
            val pRW = Expr.summon[fabric.rw.RW[p]].getOrElse(
              report.errorAndAbort(s"No RW for ${pType.show}")
            )
            val nameExpr = Expr(pName)
            '{ fabric.rw.Asable($jsonRef($nameExpr)).as[p](using $pRW) }.asTerm
        }
      }

      val methodSym = TypeRepr.of[T].baseClasses.flatMap(_.declarations)
        .find(s => s.name == methodName && s.isDefDef).get
      Select(impl.asTerm, methodSym).appliedToArgs(extractedArgs).asExprOf[rapid.Task[R]].asTerm
    }).asExprOf[fabric.Json => rapid.Task[R]]
  }
}
