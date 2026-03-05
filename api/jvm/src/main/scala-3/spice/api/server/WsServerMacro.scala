package spice.api.server

import scala.quoted.*
import spice.http.server.MutableHttpServer
import spice.net.URLPath

object WsServerMacro {
  def derive[T: Type](
    server: Expr[MutableHttpServer],
    basePath: Expr[URLPath]
  )(using Quotes): Expr[T] = {
    import quotes.reflect.*

    val tpe = TypeRepr.of[T]
    val tpeSym = tpe.typeSymbol

    if (!tpeSym.flags.is(Flags.Trait)) {
      report.errorAndAbort(s"WsServer.derive requires a trait, but got ${tpeSym.name}")
    }

    val abstractMethods = tpe.baseClasses.flatMap(_.declarations.filter { s =>
      s.isDefDef && s.flags.is(Flags.Deferred)
    }).distinctBy(_.name)

    if (abstractMethods.isEmpty) {
      report.errorAndAbort(s"Trait ${tpeSym.name} has no abstract methods to derive")
    }

    // Validate methods and collect info
    val methodInfos = abstractMethods.map { method =>
      val methodName = method.name
      val methodType = tpe.memberType(method)
      val (paramClauses, returnType) = unwrapMethodType(methodType)

      val taskSym = TypeRepr.of[rapid.Task[?]].typeSymbol
      returnType match {
        case AppliedType(base, List(inner)) if base.typeSymbol == taskSym =>
          if (!(inner =:= TypeRepr.of[Unit])) {
            report.errorAndAbort(s"WsServer method '$methodName' must return Task[Unit]")
          }
        case _ =>
          report.errorAndAbort(s"Method '$methodName' must return Task[Unit], but returns ${returnType.show}")
      }

      val allParams = paramClauses.flatten
      for ((pName, pType) <- allParams) {
        val rwType = TypeRepr.of[fabric.rw.RW].appliedTo(List(pType))
        Implicits.search(rwType) match {
          case _: ImplicitSearchSuccess =>
          case _: ImplicitSearchFailure =>
            report.errorAndAbort(s"No RW instance for ${pType.show} (parameter '$pName' of '$methodName')")
        }
      }

      (method, allParams)
    }

    // Generate serializer map entries
    val entries: List[Expr[(String, (Array[AnyRef], List[String]) => fabric.Json)]] = methodInfos.map { case (method, params) =>
      val nameExpr = Expr(method.name)
      val serializer = buildSerializer(params)
      '{ ($nameExpr, $serializer) }
    }

    val entriesExpr = Expr.ofList(entries)

    '{
      WsServerRuntime.register($server, $basePath)
      WsServerRuntime.createProxy[T](
        Class.forName(${Expr(tpeSym.fullName)}),
        $basePath,
        $entriesExpr.toMap
      )
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

  private def buildSerializer(using Quotes)(
    params: List[(String, quotes.reflect.TypeRepr)]
  ): Expr[(Array[AnyRef], List[String]) => fabric.Json] = {
    import quotes.reflect.*

    // Generate a function that takes the method's args as Array[AnyRef] and returns a Json object
    // Each arg at index i corresponds to params(i)
    val paramConversions: List[(Expr[Int], Expr[String], quotes.reflect.TypeRepr)] =
      params.zipWithIndex.map { case ((pName, pType), idx) =>
        (Expr(idx), Expr(pName), pType)
      }

    '{ (args: Array[AnyRef], _: List[String]) =>
      val pairs: List[(String, fabric.Json)] = ${
        Expr.ofList(paramConversions.map { case (idxExpr, nameExpr, pType) =>
          pType.asType match {
            case '[p] =>
              val pRW = Expr.summon[fabric.rw.RW[p]].getOrElse(
                report.errorAndAbort(s"No RW for ${pType.show}")
              )
              '{ ($nameExpr, fabric.rw.Convertible(args($idxExpr).asInstanceOf[p]).json(using $pRW)) }
          }
        })
      }
      fabric.obj(pairs*)
    }
  }
}
