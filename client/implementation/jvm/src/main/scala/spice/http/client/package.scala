package spice.http

import cats.effect.IO

import java.util.concurrent.{CompletableFuture, CompletionException}
import scala.jdk.FutureConverters.CompletionStageOps

package object client {
  implicit class CompletableFutureExtras[T](cf: CompletableFuture[T]) {
    def toIO: IO[T] = IO.fromFuture(IO(cf.asScala)).recover {
      case exc: CompletionException => throw exc.getCause
    }
  }
}