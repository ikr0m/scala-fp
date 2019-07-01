package fp.fs2

import cats.arrow.FunctionK
import cats.effect.{ContextShift, ExitCode, IO, IOApp}

import scala.concurrent.ExecutionContext
import fs2.Stream

object TranslateApp extends IOApp {
  val producerStream = Stream.range(1, 100)
  val processorStream = producerStream.map(_ * 3)

  val defaultEC = scala.concurrent.ExecutionContext.global
  val ec: ExecutionContext = ExecutionContext.fromExecutor(defaultEC)
  val contextShift2 = IO.contextShift(defaultEC)

  val fToG = new FunctionK {
    def apply[A](fa: F[A]): G[A] = {

    }
  }

  def run(args: List[String]): IO[ExitCode] = {
    processorStream.translate { a =>
      a
    }
    contextShift2.evalOn(ec) {
      processorStream
    }
  }
}