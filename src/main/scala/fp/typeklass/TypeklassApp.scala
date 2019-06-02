package fp.typeklass

// https://scalac.io/typeclasses-in-scala/

object TypeklassApp extends App {

  {
    trait Show[A] {
      def show(a: A): String
    }

    object Show {
      def show[A](a: A)(implicit sh: Show[A]): String = sh.show(a)

      def show2[A: Show](a: A): String = implicitly[Show[A]].show(a)

      val intCanShow: Show[Int] =
        new Show[Int] {
          def show(int: Int): String = s"int $int"
        }

      implicit val intCanShow2: Show[Int] =
        new Show[Int] {
          def show(int: Int): String = s"int $int"
        }

      println(intCanShow.show(30))
      println(show(31))
      println(show2(33))
    }
  }

  // --------------------

  {
    trait Show2[A] {
      def show(a: A): String
    }

    object Show2 {
      def apply[A](implicit sh: Show2[A]): Show2[A] = sh

      def show[A: Show2](a: A): String  = Show2.apply[A].show(a)
      def show2[A: Show2](a: A): String = Show2[A].show(a)

//      implicit class ShowOps[A: Show2](a: A) {
//        def show: String = Show2[A].show(a)
//      }

      implicit class ShowOps[A](a: A) {
        def show(implicit sh: Show2[A]): String = sh.show(a)
      }

      implicit val intCanShow: Show2[Int] =
        new Show2[Int] {
          def show(int: Int): String = s"int $int"
        }

      println(34.show)
    }
  }

  // -----------------

  trait Show3[A] {
    def show(a: A): String
  }

  object Show3 {

    def apply[A](implicit sh: Show3[A]): Show3[A] = sh

    object ops {
      def show[A: Show3](a: A): String = Show3[A].show(a)

      implicit class ShowOps[A: Show3](a: A) {
        def show: String = Show3[A].show(a)

        def showExp(implicit sh: Show3[A]): String = sh.show(a)
      }
    }

    implicit val intCanShow: Show3[Int] =
      int => s"int $int"

    implicit val stringCanShow: Show3[String] =
      string => s"string $string"
  }
}

object MainShow {
  import fp.typeklass.TypeklassApp.Show3
  import fp.typeklass.TypeklassApp.Show3.ops._

  case class Foo(foo: Int)

  implicit val fooShow: Show3[Foo] =
    foo => s"case class Foo(foo: ${foo.foo}"

  println(35.show)
  println(Foo(42).show)

  val hipsterString: Show3[String] =
    str => s"hipster string $str."

  println("baz".showExp) // prints: string baz
  println("baz".showExp(hipsterString)) // prints: hipster string baz
}

object SimulacrumO {
  import simulacrum._

  @typeclass trait ShowSim[A] {
    def showSim(a: A): String
  }

  object ShowSim {
    implicit val stringCanShow: ShowSim[String] =
      str => s"simulacrum string $str"

  }

  // println("bar".showSim) // doesn't work
}