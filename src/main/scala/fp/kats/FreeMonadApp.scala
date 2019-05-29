package fp.kats

import cats.free.Free
import cats.free.Free.liftF
import cats.arrow.FunctionK
import cats.{Id, InjectK, ~>}
import scala.collection.mutable
import cats.data.State
import cats.data.EitherK

// https://typelevel.org/cats/datatypes/freemonad.html

object FreeMonadApp extends App {

  // Create an ADT representing your grammar
  sealed trait KVStoreA[A]

  case class Put[T](key: String, value: T) extends KVStoreA[Unit]
  case class Get[T](key: String)           extends KVStoreA[Option[T]]
  case class Delete(key: String)           extends KVStoreA[Unit]

  // Free your ADT
  // 1. Create a Free type based on your ADT
  type KVStore[A] = Free[KVStoreA, A]

  // 2. Create smart constructors using liftF
  def put[T](key: String, value: T): KVStore[Unit] =
    liftF[KVStoreA, Unit](Put[T](key, value))

  def get[T](key: String): KVStore[Option[T]] =
    liftF[KVStoreA, Option[T]](Get[T](key))

  def delete(key: String): KVStore[Unit] =
    liftF(Delete(key))

  def update[T](key: String, f: T => T): KVStore[Unit] =
    for {
      vMaybe <- get[T](key)
      _ <- vMaybe.map(v => put[T](key, f(v))).getOrElse(Free.pure())
    } yield ()

  // 3. Build a program
  def program: KVStore[Option[Int]] =
    for {
      _ <- put("wild-cats", 2)
      _ <- update[Int]("wild-cats", _ + 12)
      _ <- put("tame-cats", 5)
      n <- get[Int]("wild-cats")
      _ <- delete("tame-cats")
    } yield n

  // 4. Write a compiler for your program
  // natural transformation
  def impureCompiler: KVStoreA ~> Id =
    new (KVStoreA ~> Id) {

      val kvs = mutable.Map.empty[String, Any]

      def apply[A](fa: KVStoreA[A]): Id[A] =
        fa match {
          case Put(key, value) =>
            println(s"put($key, $value)")
            kvs(key) = value
            ()
          case Get(key) =>
            println(s"get($key)")
            kvs.get(key).map(_.asInstanceOf[A])
          case Delete(key) =>
            println(s"delete($key)")
            kvs.remove(key)
            ()
        }
    }

  // 5. Run your program
  val result: Option[Int] = program.foldMap(impureCompiler)

  println(s"Result1: $result")

  // =======================

  // 6. Use a pure compiler (optional)
  type KVStoreState[A] = State[Map[String, Any], A]
  val pureCompiler: KVStoreA ~> KVStoreState = new (KVStoreA ~> KVStoreState) {
    def apply[A](fa: KVStoreA[A]): KVStoreState[A] =
      fa match {
        case Put(key, value) => State.modify(_.updated(key, value))
        case Get(key)        => State.inspect(_.get(key).map(_.asInstanceOf[A]))
        case Delete(key)     => State.modify(_ - key)
      }
  }

  val result2: (Map[String, Any], Option[Int]) = program.foldMap(pureCompiler).run(Map.empty).value
  println(s"Result2: $result2")

  // Composing Free monads ADTs.

  // Handles user interaction
  sealed trait Interact[A]
  case class Ask(prompt: String) extends Interact[String]
  case class Tell(msg: String) extends Interact[Unit]

  // Represents presistence operations
  sealed trait DataOp[A]
  case class AddCat(a: String) extends DataOp[Unit]
  case class GetAllCats() extends DataOp[List[String]]

  type CatsApp[A] = EitherK[DataOp, Interact, A]

  class Interacts[F[_]](implicit I: InjectK[Interact, F]) {
    def tell(msg: String): Free[F, Unit] = Free.inject[Interact, F](Tell(msg))
    def ask(prompt: String): Free[F, String] = Free.inject[Interact, F](Ask(prompt))
  }

  object Interacts {
    implicit def interacts[F[_]](implicit I: InjectK[Interact, F]): Interacts[F] = new Interacts[F]

  }

}

