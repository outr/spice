package spice.api

import scala.quoted.*
import spice.net.{URL, URLPath}

object ApiClientMacro {
  def derive[T: Type](baseUrl: Expr[URL])(using Quotes): Expr[T] = {
    import quotes.reflect.*

    val tpe = TypeRepr.of[T]
    val tpeSym = tpe.typeSymbol

    if (!tpeSym.flags.is(Flags.Trait)) {
      report.errorAndAbort(s"ApiClient.derive requires a trait, but got ${tpeSym.name}")
    }

    val abstractMethods = tpe.baseClasses.flatMap(_.declarations.filter { s =>
      s.isDefDef && s.flags.is(Flags.Deferred)
    }).distinctBy(_.name)

    if (abstractMethods.isEmpty) {
      report.errorAndAbort(s"Trait ${tpeSym.name} has no abstract methods to derive")
    }

    val methodInfos = abstractMethods.map { method =>
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

      validateRW(responseType, s"return type of '$methodName'")
      for {
        clause <- paramClauses
        (paramName, paramType) <- clause
      } validateRW(paramType, s"parameter '$paramName' of '$methodName'")

      (method, paramClauses, responseType)
    }

    buildProxy[T](baseUrl, methodInfos)
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

  private def validateRW(using Quotes)(tpe: quotes.reflect.TypeRepr, context: String): Unit = {
    import quotes.reflect.*
    val rwType = TypeRepr.of[fabric.rw.RW].appliedTo(List(tpe))
    Implicits.search(rwType) match {
      case _: ImplicitSearchSuccess => // ok
      case _: ImplicitSearchFailure =>
        report.errorAndAbort(s"No RW instance found for ${tpe.show} ($context)")
    }
  }

  private def isCaseClass(using Quotes)(tpe: quotes.reflect.TypeRepr): Boolean = {
    import quotes.reflect.*
    val sym = tpe.typeSymbol
    sym.flags.is(Flags.Case) && sym.isClassDef
  }

  private def buildProxy[T: Type](using Quotes)(
    baseUrl: Expr[URL],
    methods: List[(quotes.reflect.Symbol, List[List[(String, quotes.reflect.TypeRepr)]], quotes.reflect.TypeRepr)]
  ): Expr[T] = {
    import quotes.reflect.*

    val parents = List(TypeTree.of[Object], TypeTree.of[T])
    val cls = Symbol.newClass(
      Symbol.spliceOwner,
      "ApiClientProxy",
      List(TypeRepr.of[Object], TypeRepr.of[T]),
      decls = { cls =>
        methods.map { case (method, paramClauses, returnType) =>
          val taskRetType = TypeRepr.of[rapid.Task].appliedTo(List(returnType))
          val methodTpe = paramClauses.foldRight(taskRetType: TypeRepr) { (params, acc) =>
            MethodType(params.map(_._1))(_ => params.map(_._2), _ => acc)
          }
          Symbol.newMethod(cls, method.name, methodTpe, Flags.Override, Symbol.noSymbol)
        }
      },
      selfType = None
    )

    val methodDefs = methods.map { case (origMethod, paramClauses, responseType) =>
      val newMethodSym = cls.declarations.find(s => s.name == origMethod.name && s.isDefDef).get
      DefDef(newMethodSym, { argss =>
          val methodName = origMethod.name
          val allParams = paramClauses.flatten
          val flatArgs: List[Tree] = argss.flatten

          if (allParams.isEmpty) {
            Some(mkGetCall(baseUrl, methodName, responseType))
          } else if (allParams.size == 1 && isCaseClass(allParams.head._2)) {
            Some(mkRestfulCall(baseUrl, methodName, allParams.head._2, responseType, flatArgs.head))
          } else {
            Some(mkJsonCall(baseUrl, methodName, responseType, allParams.zip(flatArgs)))
          }
        })
    }


    val clsDef = ClassDef(cls, parents, body = methodDefs)
    Block(
      List(clsDef),
      Typed(Apply(Select(New(TypeIdent(cls)), cls.primaryConstructor), Nil), TypeTree.of[T])
    ).asExprOf[T]
  }

  private def mkGetCall(using Quotes)(
    baseUrl: Expr[URL],
    methodName: String,
    responseType: quotes.reflect.TypeRepr
  ): quotes.reflect.Term = {
    import quotes.reflect.*
    responseType.asType match {
      case '[r] =>
        val rw = Expr.summon[fabric.rw.RW[r]].getOrElse(
          report.errorAndAbort(s"No RW for ${responseType.show}")
        )
        val name = Expr(methodName)
        '{ ApiClientRuntime.doGet[r]($baseUrl, $name)(using $rw) }.asTerm
    }
  }

  private def mkRestfulCall(using Quotes)(
    baseUrl: Expr[URL],
    methodName: String,
    requestType: quotes.reflect.TypeRepr,
    responseType: quotes.reflect.TypeRepr,
    arg: quotes.reflect.Tree
  ): quotes.reflect.Term = {
    import quotes.reflect.*
    (requestType.asType, responseType.asType) match {
      case ('[req], '[res]) =>
        val reqRW = Expr.summon[fabric.rw.RW[req]].getOrElse(
          report.errorAndAbort(s"No RW for ${requestType.show}")
        )
        val resRW = Expr.summon[fabric.rw.RW[res]].getOrElse(
          report.errorAndAbort(s"No RW for ${responseType.show}")
        )
        val typedArg = arg.asExprOf[req]
        val name = Expr(methodName)
        '{ ApiClientRuntime.doRestful[req, res]($baseUrl, $name, $typedArg)(using $reqRW, $resRW) }.asTerm
      case _ => throw new RuntimeException("Inconceivable!")
    }
  }

  private def mkJsonCall(using Quotes)(
    baseUrl: Expr[URL],
    methodName: String,
    responseType: quotes.reflect.TypeRepr,
    params: List[((String, quotes.reflect.TypeRepr), quotes.reflect.Tree)]
  ): quotes.reflect.Term = {
    import quotes.reflect.*
    responseType.asType match {
      case '[r] =>
        val resRW = Expr.summon[fabric.rw.RW[r]].getOrElse(
          report.errorAndAbort(s"No RW for ${responseType.show}")
        )
        val jsonPairs: Expr[List[(String, fabric.Json)]] = Expr.ofList(
          params.map { case ((pName, pType), argTree) =>
            pType.asType match {
              case '[p] =>
                val pRW = Expr.summon[fabric.rw.RW[p]].getOrElse(
                  report.errorAndAbort(s"No RW for ${pType.show}")
                )
                val typedArg = argTree.asExprOf[p]
                val nameExpr = Expr(pName)
                '{ ($nameExpr, fabric.rw.Convertible($typedArg).json(using $pRW)) }
            }
          }
        )
        val name = Expr(methodName)
        '{ ApiClientRuntime.doJson[r]($baseUrl, $name, $jsonPairs)(using $resRW) }.asTerm
    }
  }
}
