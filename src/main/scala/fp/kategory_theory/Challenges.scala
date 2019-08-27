package fp.kategory_theory

import scala.util.Random

// Category Theory for Programmers
object Challenges extends App {

  // 1.4 Challenges
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

  // 2.7 Challenges (Impure version)
  {

    //    val cache = new scala.collection.mutable.LongMap[BigInt]()
    //    def memoize(f: Long => BigInt): Long => BigInt = n => {
    //      cache.get(n) match {
    //        case Some(value) =>
    //          println(s"Using cache $n")
    //          value
    //        case None =>
    //          println(s"Computing $n")
    //          val value = f(n)
    //          cache.update(n, value)
    //          value
    //      }
    //    }
    //    def factWithCache = memoize(fact)

    class Mem[A, B] {
      private val cache = new scala.collection.mutable.HashMap[A, B]
      val memoize: (A => B) => A => B = f => n => cache.getOrElseUpdate(n, f(n))
    }

    def checkMemoize[A, B](listElements: List[A], f: A => B): Unit = {
      listElements.foreach { n =>
        println("--------------------------------")
        val t1 = System.nanoTime()
        val result = f(n)
        val t2 = System.nanoTime()
        println(s"Elapsed time: ${(t2 - t1) / 1000f} ms; $n -> $result")
      }
    }

    // -------------

    def fact(n: Int): BigInt = (1L to n).foldLeft(BigInt(1))(_ * BigInt(_))
    val factWithCache = new Mem[Int, BigInt].memoize(fact)
    val listNumbers: List[Int] = List(23, 23, 30, 30, 30, 150, 80, 150, 80)

    checkMemoize(listNumbers, factWithCache)

    // --------------

    println("Checking random WITHOUT seed")
    val intRandomWrapper: Unit => Int = _ => Random.nextInt()
    val rndWithCache = new Mem[Unit, Int].memoize(intRandomWrapper)
    checkMemoize(List((), (), ()), rndWithCache)

    // ------------
    println("Checking random with seed")
    val randomWithSeed: Int => Int = Random.nextInt
    val randomWithCache = new Mem[Int, Int].memoize(randomWithSeed)
    checkMemoize(listNumbers, randomWithCache)

    // ------------
    val isTrue: Boolean => Boolean = b => b
    val negate: Boolean => Boolean = b => !b
  }

  {
    def fact(n: Int): BigInt = (1L to n).foldLeft(BigInt(1))(_ * BigInt(_))


    class Memo[A, B] {
      type MemoState = Map[A, B]

      def memoize(f: A => B): (MemoState, (MemoState, A) => (MemoState, B)) = {
        val func = (state: MemoState, a: A) => {
          state.get(a) match {
            case Some(value) =>
              (state, value)
            case None =>
              val value = f(a)
              (state.updated(a, value), value)
          }
        }
        (Map.empty[A, B], func)
      }
    }


    val (state1, mf) = new Memo().memoize(fact)
    val (state2, v1) = mf(state1, 56)
    println(s"${System.nanoTime()}, v1: $v1")
    val (state3, v2) = mf(state2, 99)
    println(s"${System.nanoTime()}, v2: $v2")
    val (state4, v3) = mf(state3, 56)
    println(s"${System.nanoTime()}, v3: $v3")
  }

}