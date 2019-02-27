package metius.integer

import org.scalameter.Bench.LocalTime
import org.scalameter.Gen

object IntegerPerformance extends LocalTime {

  private val sizes: Gen[Int] = Gen.range("size")(from = 200000, upto = 1000000, hop = 200000)

  private val ranges = for {
    size <- sizes
  } yield 0 until size

  import HashOperations._

  var n: Long = 0L
  performance of "murmurhash3" in {
    measure method "implicit class (anyval)" in {
      using(ranges) in {
        _ foreach { x =>
          (x * 10L).murmurHash3
        }
      }
    }

    measure method "compare to inline" in {
      using(ranges) in {
        _ foreach { x =>
          var z = x * 10L
          z = (z ^ (z >>> 33)) * 0xff51afd7ed558ccdL
          z = (z ^ (z >>> 33)) * 0xc4ceb9fe1a85ec53L
          z ^ (z >>> 33)
        }
      }
    }

    measure method "compare to inline 2" in {
      using(ranges) in {
        _ foreach { x =>
          var z = x * 10L
          z ^= z >>> 33
          z *= 0xff51afd7ed558ccdL
          z ^= z >>> 33
          z *= 0xc4ceb9fe1a85ec53L
          z ^ z >>> 33
        }
      }
    }
  }

  measure method "implicit class (anyval) bis" in {
    using(ranges) in {
      _ foreach { x =>
        (x * 10L).murmurHash3
      }
    }
  }

  performance of "mixer" in {
    measure method "murmurhash3 (long)" in {
      using(ranges) in {
        _ foreach { x => Mixer.murmurHash3(x * 10L)
        }
      }
    }
  }

  performance of "baseline" in {
    measure method "toLong" in {
      using(ranges) in {
        _ foreach { x => x.toLong
        }
      }
    }
  }

}
