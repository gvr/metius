package metius.real

import org.scalameter.Bench.LocalTime
import org.scalameter.Gen

object PolynomialPerformance extends LocalTime {

  private val n = 100

  private val sizes = Gen.range("size")(20000 / n, 100000 / n, 20000 / n)

  private val ranges = for {
    size <- sizes
  } yield 0 until size

  private def random(limit: Double): Double = 2 * limit * (0.5 - math.random())

  private def randomParameter(): Double = random(1000.0)

  private def randomVariable(): Double = random(10.0)

  private var sum = 0.0

  performance of "polynomial degree 3" in {
    measure method "plain" in {
      using(ranges) in {
        _ foreach { _ =>
          val a0 = randomParameter()
          val a1 = randomParameter()
          val a2 = randomParameter()
          val a3 = randomParameter()
          1 to n foreach { _ =>
            val x = randomVariable()
            sum += a0 + x * (a1 + x * (a2 + x * a3))
          }
        }
      }
    }
  }

  performance of "polynomial degree 3" in {
    measure method "apply" in {
      using(ranges) in {
        _ foreach { _ =>
          val a0 = randomParameter()
          val a1 = randomParameter()
          val a2 = randomParameter()
          val a3 = randomParameter()
          val p = Polynomial(a0, a1, a2, a3)
          1 to n foreach { _ =>
            val x = randomVariable()
            sum += p(x)
          }
        }
      }
    }
  }

  performance of "polynomial degree 3" in {
    measure method "horner" in {
      using(ranges) in {
        _ foreach { _ =>
          val a0 = randomParameter()
          val a1 = randomParameter()
          val a2 = randomParameter()
          val a3 = randomParameter()
          val p = Polynomial.horner(List(a0, a1, a2, a3))
          1 to n foreach { _ =>
            val x = randomVariable()
            sum += p(x)
          }
        }
      }
    }
  }

  performance of "polynomial degree 15" in {
    measure method "plain" in {
      using(ranges) in {
        _ foreach { _ =>
          val a0 = randomParameter()
          val a1 = randomParameter()
          val a2 = randomParameter()
          val a3 = randomParameter()
          val a4 = randomParameter()
          val a5 = randomParameter()
          val a6 = randomParameter()
          val a7 = randomParameter()
          val a8 = randomParameter()
          val a9 = randomParameter()
          val a10 = randomParameter()
          val a11 = randomParameter()
          val a12 = randomParameter()
          val a13 = randomParameter()
          val a14 = randomParameter()
          val a15 = randomParameter()
          1 to n foreach { _ =>
            val x = randomVariable()
            sum += a0 + x * (a1 + x * (a2 + x * (a3 + x * (a4 + x * (a5 + x * (a6 + x * (a7 + x * (a8 + x * (a9 +
              x * (a10 + x * (a11 + x * (a12 + x * (a13 + x * (a14 + x * a15))))))))))))))
          }
        }
      }
    }
  }

  performance of "polynomial degree 15" in {
    measure method "apply" in {
      using(ranges) in {
        _ foreach { _ =>
          val a0 = randomParameter()
          val a1 = randomParameter()
          val a2 = randomParameter()
          val a3 = randomParameter()
          val a4 = randomParameter()
          val a5 = randomParameter()
          val a6 = randomParameter()
          val a7 = randomParameter()
          val a8 = randomParameter()
          val a9 = randomParameter()
          val a10 = randomParameter()
          val a11 = randomParameter()
          val a12 = randomParameter()
          val a13 = randomParameter()
          val a14 = randomParameter()
          val a15 = randomParameter()
          val p = Polynomial(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15)
          1 to n foreach { _ =>
            val x = randomVariable()
            sum += p(x)
          }
        }
      }
    }
  }

  performance of "polynomial degree 15" in {
    measure method "horner" in {
      using(ranges) in {
        _ foreach { _ =>
          val a0 = randomParameter()
          val a1 = randomParameter()
          val a2 = randomParameter()
          val a3 = randomParameter()
          val a4 = randomParameter()
          val a5 = randomParameter()
          val a6 = randomParameter()
          val a7 = randomParameter()
          val a8 = randomParameter()
          val a9 = randomParameter()
          val a10 = randomParameter()
          val a11 = randomParameter()
          val a12 = randomParameter()
          val a13 = randomParameter()
          val a14 = randomParameter()
          val a15 = randomParameter()
          val p = Polynomial.horner(Seq(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15))
          1 to n foreach { _ =>
            val x = randomVariable()
            sum += p(x)
          }
        }
      }
    }
  }

  performance of "polynomial degree 15" in {
    measure method "horner, list" in {
      using(ranges) in {
        _ foreach { _ =>
          val a0 = randomParameter()
          val a1 = randomParameter()
          val a2 = randomParameter()
          val a3 = randomParameter()
          val a4 = randomParameter()
          val a5 = randomParameter()
          val a6 = randomParameter()
          val a7 = randomParameter()
          val a8 = randomParameter()
          val a9 = randomParameter()
          val a10 = randomParameter()
          val a11 = randomParameter()
          val a12 = randomParameter()
          val a13 = randomParameter()
          val a14 = randomParameter()
          val a15 = randomParameter()
          val p = Polynomial.horner(List(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15))
          1 to n foreach { _ =>
            val x = randomVariable()
            sum += p(x)
          }
        }
      }
    }
  }

  performance of "polynomial degree 15" in {
    measure method "horner, vector" in {
      using(ranges) in {
        _ foreach { _ =>
          val a0 = randomParameter()
          val a1 = randomParameter()
          val a2 = randomParameter()
          val a3 = randomParameter()
          val a4 = randomParameter()
          val a5 = randomParameter()
          val a6 = randomParameter()
          val a7 = randomParameter()
          val a8 = randomParameter()
          val a9 = randomParameter()
          val a10 = randomParameter()
          val a11 = randomParameter()
          val a12 = randomParameter()
          val a13 = randomParameter()
          val a14 = randomParameter()
          val a15 = randomParameter()
          val p = Polynomial.horner(Vector(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15))
          1 to n foreach { _ =>
            val x = randomVariable()
            sum += p(x)
          }
        }
      }
    }
  }

  performance of "polynomial degree 15" in {
    measure method "horner, array" in {
      using(ranges) in {
        _ foreach { _ =>
          val a0 = randomParameter()
          val a1 = randomParameter()
          val a2 = randomParameter()
          val a3 = randomParameter()
          val a4 = randomParameter()
          val a5 = randomParameter()
          val a6 = randomParameter()
          val a7 = randomParameter()
          val a8 = randomParameter()
          val a9 = randomParameter()
          val a10 = randomParameter()
          val a11 = randomParameter()
          val a12 = randomParameter()
          val a13 = randomParameter()
          val a14 = randomParameter()
          val a15 = randomParameter()
          val p = Polynomial.horner(Array(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15))
          1 to n foreach { _ =>
            val x = randomVariable()
            sum += p(x)
          }
        }
      }
    }
  }

}
