package fp.kategory_theory

// Category Theory for Programmers
object Challenges extends App {

  // 1.4.1 Challenges
  {
    def identity[A](a: A): A = a

    def compose[A, B, C](f: A => B, g: B => C): A => C = (a: A) =>
        g(f(a))

    val plus2: Int => Int = i => i + 2

    val f1 = compose(plus2, identity[Int])
    val f2 = compose(identity[Int], plus2)
    val r1 = plus2(3)
    val r2 = f1(3)
    val r3 = f2(3)
    println(s"r1: $r1, r2: $r2, r3: $r3")
  }

}