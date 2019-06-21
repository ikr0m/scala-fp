package fp.taglessfinal

import cats.Monad
import monix.eval.Task

import scala.io.StdIn

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

}