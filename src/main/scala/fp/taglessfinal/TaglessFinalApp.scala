/*
package fp.taglessfinal

import cats.Monad
import monix.eval.Task
import cats.syntax.functor._
import cats.syntax.flatMap._
import cats.implicits._
import cats.Parallel

import cats.free._
import cats._

import scala.io.StdIn

// scale.bythebay.io Luka Jacobowitz, Building a Tagless Final DSL for WebGL in Scala

object TaglessFinalApp extends App {

  trait Console[F[_]] {
    def printLine(s: String): F[Unit]

    def readLine: F[String]
  }

  def program[F[_]: Monad](C: Console[F]) = for {
    _     <- C.printLine("Please enter your name")
    name  <- C.readLine
    _     <- C.printLine(s"You entered $name")
  } ()

  object ConsoleTaskInterpreter extends Console[Task] {
    def printLine(s: String): Task[Unit] =
      Task(println(s))
    def readLine: Task[String] =
      Task(StdIn.readLine())
  }

  program(ConsoleTaskInterpreter).runAsync

  // Combining algebra

  trait KVStore[F[_]] {
    def put[A](key: String, a: A): F[Unit]
    def get[A](key: String): F[Option[A]]
  }

  def program2[F[_]: FlatMap](C: Console[F], K: KVStore[F]) = for {
    _     <- C.printLine("Please enter your name")
    name  <- C.readLine
    _     <- K.put("name", name)
  } yield ()

  trait Prompt[F[_]] {
    def prompt(msg: String): F[String]
  }

  class PromptConsoleInterpreter[F[_]: FlatMap](C: Console[F])
    extends Prompt[F] {
      def prompt(msg: String): F[String] = for {
        _ <- C.printLine(msg)
        s <- C.readLine
      } yield s
  }

  def program[M[_]: FlatMap, F[_]](K: KVStore[F])(implicit P: Parallel[M, F]) =
    for {
      _ <- K.put("A", a)
      x <- (K.get("B"), K.get("C")).parMapN(f)
    } yield x

  program2(new PromptConsoleInterpreter).runAsync

}
*/