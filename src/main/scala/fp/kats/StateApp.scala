package fp.kats

// https://typelevel.org/cats/datatypes/state.html

object StateApp extends App {

  final case class Robot(
                        id: Long,
                        sentient: Boolean,
                        name: String,
                        model: String
                        ){}

  // Old style
  {
    val rng = new scala.util.Random(0L)

    def createRobot(): Robot = {
      val id = rng.nextLong()
      val sentient = rng.nextBoolean()
      val isCatherine = rng.nextBoolean()
      val name = if (isCatherine) "Catherine" else "Carlos"
      val isReplicant = rng.nextBoolean()
      val model = if (isReplicant) "replicant" else "borg"
      Robot(id, sentient, name, model)
    }

    val robot = createRobot()

    println(s"robot: $robot")
  }

  // With Seed

  final case class Seed(long: Long) {
    def next = Seed(long * 6364136223846793005L + 1442695040888963407L)
  }

  val initialSeed = Seed(13L)

  {
    def nextBoolean(seed: Seed): (Seed, Boolean) = {
      (seed.next, seed.long >= 0L)
    }

    def nextLong(seed: Seed): (Seed, Long) = {
      (seed.next, seed.long)
    }

    def createRobot2(seed: Seed): Robot = {
      val (seed1, id) = nextLong(seed)
      val (seed2, sentient) = nextBoolean(seed1)
      val (seed3, isCatherine) = nextBoolean(seed2)
      val name = if (isCatherine) "Catherine" else "Carlos"
      val (seed4, isReplicant) = nextBoolean(seed3)
      val model = if (isReplicant) "replicant" else "borg"
      Robot(id, sentient, name, model)
    }

    val robot2 = createRobot2(initialSeed)

    println(s"robot2: $robot2")
  }

  // With State

  import cats.data.State

  val nextLong: State[Seed, Long] = State(seed => (seed.next, seed.long))

  val nextBoolean: State[Seed, Boolean] = nextLong.map(long => long > 0)

  val createRobot3: State[Seed, Robot] =
    for {
      id          <- nextLong
      sentient    <- nextBoolean
      isCatherine <- nextBoolean
      name        = if (isCatherine) "Catherine" else "Carlos"
      isReplicant <- nextBoolean
      model       = if (isReplicant) "replicant" else "borg"
    } yield Robot(id, sentient, name, model)

  val (finalState, robot3) = createRobot3.run(initialSeed).value
  println(s"finalState: $finalState, robot3: $robot3")

  val robot4 = createRobot3.runA(initialSeed).value
  println(s"robot4: $robot4")

  // With State and referential transparent

  val createRobot4: State[Seed, Robot] = {
    val b = nextBoolean

    for {
      id <- nextLong
      sentient <- b
      isCatherine <- b
      name = if (isCatherine) "Catherine" else "Carlos"
      isReplicant <- b
      model = if (isReplicant) "replicant" else "borg"
    } yield Robot(id, sentient, name, model)
  }

  val robot5 = createRobot4.runA(initialSeed).value
  println(s"robot5: $robot5")

}
