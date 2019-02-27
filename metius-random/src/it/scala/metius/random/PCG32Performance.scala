package metius.random

import org.scalameter.Bench.LocalTime
import org.scalameter.Gen

object PCG32Performance extends LocalTime {

  private val sizes: Gen[Int] = Gen.range("size")(200000, 1000000, 200000)

  private val ranges = for {
    size <- sizes
  } yield 0 until size

  performance of "baseline" in {

    measure method "noop" in {
      using(ranges) in {
        _ foreach { x =>
          x
        }
      }
    }

  }

  performance of "PCG32" in {

    val immutablePrng = PCG32.init(1L, 1L)
    var next = immutablePrng
    measure method "immutable" in {
      using(ranges) in {
        _ foreach { _ =>
          next = immutablePrng.next
          next.get
        }
      }
    }

    val mutablePrng = PCG32.randomInts(1L, 1L)
    measure method "mutable" in {
      using(ranges) in {
        _ foreach { _ =>
          mutablePrng.next()
        }
      }
    }
  }

  performance of "java standard library" in {

    val javaRandom = new java.util.Random(1L)
    measure method "Random" in {
      using(ranges) in {
        _ foreach { _ =>
          javaRandom.nextLong()
        }
      }
    }

    val javaSplittableRandom = new java.util.SplittableRandom(1L)
    measure method "SplittableRandom" in {
      using(ranges) in {
        _ foreach { _ =>
          javaSplittableRandom.nextLong()
        }
      }
    }
  }

}
