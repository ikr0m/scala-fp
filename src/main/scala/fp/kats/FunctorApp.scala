package fp.kats

// Book 'Scala with Cats' p57

import scala.language.higherKinds
import cats.Functor
import cats.instances.list._    // for Functor
import cats.instances.option._ // for Functor

object FunctorApp extends App {
  val list1 = List(1, 2, 3)

  val list2 = Functor[List].map(list1)(_ * 2)

  val option1 = Option(123)

  val option2 = Functor[Option].map(option1)(_.toString)

  println(s"list1: $list1, list2: $list2, option1: $option1, option2: $option2")

  val func = (x: Int) => x + 1

  val liftedFunc = Functor[Option].lift(func)

  val liftedFuncResult = liftedFunc(Option(1))

  println(s"liftedFuncResult: $liftedFuncResult")
}