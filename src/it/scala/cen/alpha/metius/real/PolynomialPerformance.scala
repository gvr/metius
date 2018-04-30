package cen.alpha.metius.real

import com.github.gvr.metius.real.Polynomial
import org.scalameter.Bench.LocalTime
import org.scalameter.Gen

object PolynomialPerformance extends LocalTime {

  private val n = 100

  private val sizes = Gen.range("size")(20000 / n, 100000 / n, 20000 / n)

  private val ranges = for {
    size <- sizes
  } yield 0 until size


  def random(limit: Double): Double = 2 * limit * (0.5 - math.random())

  private var sum = 0.0

  performance of "polynomial degree 3" in {
    measure method "plain" in {
      using(ranges) in {
        _ foreach { _ =>
          val a0 = random(1000.0)
          val a1 = random(1000.0)
          val a2 = random(1000.0)
          val a3 = random(1000.0)
          1 to n foreach { _ =>
            val x = random(10.0)
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
          val a0 = random(1000.0)
          val a1 = random(1000.0)
          val a2 = random(1000.0)
          val a3 = random(1000.0)
          val p = Polynomial(a0, a1, a2, a3)
          1 to n foreach { _ =>
            val x = random(10.0)
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
          val a0 = random(1000.0)
          val a1 = random(1000.0)
          val a2 = random(1000.0)
          val a3 = random(1000.0)
          val p = Polynomial.horner(List(a0, a1, a2, a3))
          1 to n foreach { _ =>
            val x = random(10.0)
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
          val a0 = random(1000.0)
          val a1 = random(1000.0)
          val a2 = random(1000.0)
          val a3 = random(1000.0)
          val a4 = random(1000.0)
          val a5 = random(1000.0)
          val a6 = random(1000.0)
          val a7 = random(1000.0)
          val a8 = random(1000.0)
          val a9 = random(1000.0)
          val a10 = random(1000.0)
          val a11 = random(1000.0)
          val a12 = random(1000.0)
          val a13 = random(1000.0)
          val a14 = random(1000.0)
          val a15 = random(1000.0)
          1 to n foreach { _ =>
            val x = random(10.0)
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
          val a0 = random(1000.0)
          val a1 = random(1000.0)
          val a2 = random(1000.0)
          val a3 = random(1000.0)
          val a4 = random(1000.0)
          val a5 = random(1000.0)
          val a6 = random(1000.0)
          val a7 = random(1000.0)
          val a8 = random(1000.0)
          val a9 = random(1000.0)
          val a10 = random(1000.0)
          val a11 = random(1000.0)
          val a12 = random(1000.0)
          val a13 = random(1000.0)
          val a14 = random(1000.0)
          val a15 = random(1000.0)
          val p = Polynomial(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15)
          1 to n foreach { _ =>
            val x = random(10.0)
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
          val a0 = random(1000.0)
          val a1 = random(1000.0)
          val a2 = random(1000.0)
          val a3 = random(1000.0)
          val a4 = random(1000.0)
          val a5 = random(1000.0)
          val a6 = random(1000.0)
          val a7 = random(1000.0)
          val a8 = random(1000.0)
          val a9 = random(1000.0)
          val a10 = random(1000.0)
          val a11 = random(1000.0)
          val a12 = random(1000.0)
          val a13 = random(1000.0)
          val a14 = random(1000.0)
          val a15 = random(1000.0)
          val p = Polynomial.horner(Seq(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15))
          1 to n foreach { _ =>
            val x = random(10.0)
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
          val a0 = random(1000.0)
          val a1 = random(1000.0)
          val a2 = random(1000.0)
          val a3 = random(1000.0)
          val a4 = random(1000.0)
          val a5 = random(1000.0)
          val a6 = random(1000.0)
          val a7 = random(1000.0)
          val a8 = random(1000.0)
          val a9 = random(1000.0)
          val a10 = random(1000.0)
          val a11 = random(1000.0)
          val a12 = random(1000.0)
          val a13 = random(1000.0)
          val a14 = random(1000.0)
          val a15 = random(1000.0)
          val p = Polynomial.horner(List(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15))
          1 to n foreach { _ =>
            val x = random(10.0)
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
          val a0 = random(1000.0)
          val a1 = random(1000.0)
          val a2 = random(1000.0)
          val a3 = random(1000.0)
          val a4 = random(1000.0)
          val a5 = random(1000.0)
          val a6 = random(1000.0)
          val a7 = random(1000.0)
          val a8 = random(1000.0)
          val a9 = random(1000.0)
          val a10 = random(1000.0)
          val a11 = random(1000.0)
          val a12 = random(1000.0)
          val a13 = random(1000.0)
          val a14 = random(1000.0)
          val a15 = random(1000.0)
          val p = Polynomial.horner(Vector(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15))
          1 to n foreach { _ =>
            val x = random(10.0)
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
          val a0 = random(1000.0)
          val a1 = random(1000.0)
          val a2 = random(1000.0)
          val a3 = random(1000.0)
          val a4 = random(1000.0)
          val a5 = random(1000.0)
          val a6 = random(1000.0)
          val a7 = random(1000.0)
          val a8 = random(1000.0)
          val a9 = random(1000.0)
          val a10 = random(1000.0)
          val a11 = random(1000.0)
          val a12 = random(1000.0)
          val a13 = random(1000.0)
          val a14 = random(1000.0)
          val a15 = random(1000.0)
          val p = Polynomial.horner(Array(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15))
          1 to n foreach { _ =>
            val x = random(10.0)
            sum += p(x)
          }
        }
      }
    }
  }

}
