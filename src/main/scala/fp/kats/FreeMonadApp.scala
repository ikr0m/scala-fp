package fp.kats

import cats.data.{EitherK, State}
import cats.free.Free
import cats.free.Free.liftF
import cats.{Id, InjectK, ~>}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.language.higherKinds

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

  println("---------------------------------------------")

  // Composing Free monads ADTs.

  // Handles user interaction
  sealed trait Interact[A]
  case class Ask(prompt: String) extends Interact[String]
  case class Tell(msg: String) extends Interact[Unit]

  // Represents persistence operations
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

  class DataSource[F[_]](implicit I: InjectK[DataOp, F]) {
    def addCat(a: String): Free[F, Unit] = Free.inject[DataOp, F](AddCat(a))
    def getAllCats: Free[F, List[String]] = Free.inject[DataOp, F](GetAllCats())
  }

  object DataSource {
    implicit def dataSource[F[_]](implicit I: InjectK[DataOp, F]): DataSource[F] = new DataSource[F]
  }

  def program2(implicit I: Interacts[CatsApp], D: DataSource[CatsApp]): Free[CatsApp, Unit] = {
    import D._
    import I._

    for {
      cat   <- ask("What's the kitty's name?")
      _     <- addCat(cat)
      cats  <- getAllCats
      _     <- tell(cats.toString)
    } yield ()
  }

  object ConsoleCatsInterpreter extends (Interact ~> Id) {
    def apply[A](i: Interact[A]) = i match {
      case Ask(prompt) =>
        println(prompt)
        readLine()
      case Tell(msg) =>
        println(msg)
    }
  }

  object InMemoryDatasourceInterpreter extends (DataOp ~> Id) {
    private[this] val memDataSet = new ListBuffer[String]

    def apply[A](fa: DataOp[A]) = fa match {
      case AddCat(a) =>
        memDataSet.append(a)
        ()
      case GetAllCats() => memDataSet.toList
    }
  }

  val interpreter: CatsApp ~> Id = InMemoryDatasourceInterpreter or ConsoleCatsInterpreter

  val evaled: Unit = program2.foldMap(interpreter)

  println("---------------------------------------------")

  // FreeT
  import cats.free._
  import cats._
  import cats.data._

  sealed abstract class Teletype[A] extends Product with Serializable

  final case class WriteLine(line: String) extends Teletype[Unit]
  final case class ReadLine(prompt: String) extends Teletype[String]

  type TeletypeT[M[_], A] = FreeT[Teletype, M, A]

  type Log = List[String]

  type TeletypeState[A] = State[List[String], A]

  object TeletypeOps {
    def writeLine(line: String): TeletypeT[TeletypeState, Unit] =
      FreeT.liftF[Teletype, TeletypeState, Unit](WriteLine(line))

    def readLine(prompt: String): TeletypeT[TeletypeState, String] =
      FreeT.liftF[Teletype, TeletypeState, String](ReadLine(prompt))

    def log(s: String): TeletypeT[TeletypeState, Unit] =
      FreeT.liftT[Teletype, TeletypeState, Unit](State.modify(s :: _))
  }

  def program3: TeletypeT[TeletypeState, Unit] = {
    for {
      userSaid <- TeletypeOps.readLine("what's up?")
      _ <- TeletypeOps.log(s"user said: $userSaid")
      _ <- TeletypeOps.writeLine("thanks, see u soon!")
    } yield ()
  }

  def interpreter2 = new (Teletype ~> TeletypeState) {
    def apply[A](fa: Teletype[A]): TeletypeState[A] = {
      fa match {
        case ReadLine(prompt) =>
          println(prompt)
          val userInput = "hanging in here"
          StateT.pure[Eval, List[String], A](userInput)
        case WriteLine(line) =>
          StateT.pure[Eval, List[String], A](println(line))
      }
    }
  }

  import TeletypeOps._

  val state = program3.foldMap(interpreter2)
  val initialState = Nil
  val (stored, _) = state.run(initialState).value

  println(s"stored: $stored")

  println("---------------------------------------------")

  import cats.implicits._
  import scala.util.Try

  sealed trait Ctx[A]

  case class Action(value: Int) extends Ctx[Int]

  def op1: FreeT[Ctx, Option, Int] =
    FreeT.liftF[Ctx, Option, Int](Action(7))

  def op2: FreeT[Ctx, Option, Int] =
    FreeT.liftT[Ctx, Option, Int](Some(4))

  def op3: FreeT[Ctx, Option, Int] =
    FreeT.pure[Ctx, Option, Int](1)

  val opComplete: FreeT[Ctx, Option, Int] =
    for {
      a <- op1
      b <- op2
      c <- op3
    } yield a + b + c

  // Interpreters

  type OptTry[A] = OptionT[Try, A]

  def tryInterpreter: Ctx ~> OptTry = new (Ctx ~> OptTry) {
    def apply[A](fa: Ctx[A]): OptTry[A] = {
      fa match {
        case Action(value) =>
          OptionT.liftF(Try(value))
      }
    }
  }

  def optTryLift: Option ~> OptTry = new (Option ~> OptTry) {
    def apply[A](fa: Option[A]): OptTry[A] = {
      fa match {
        case Some(value) =>
          OptionT(Try(Option(value)))
        case None =>
          OptionT.none
      }
    }
  }

  val hoisted = opComplete.hoist(optTryLift)
  val evaluated = hoisted.foldMap(tryInterpreter)
  val result4 = evaluated.value

  println(s"result4: $result4")
}