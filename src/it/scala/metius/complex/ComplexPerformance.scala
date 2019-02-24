package metius.complex

import org.scalameter.Bench.LocalTime
import org.scalameter.Gen

final case class TestComplex(re: Double, im: Double) {

  def +(that: TestComplex): TestComplex =
    TestComplex(this.re + that.re, this.im + that.im)

  def -(that: TestComplex): TestComplex =
    TestComplex(this.re - that.re, this.im - that.im)

  def *(that: TestComplex): TestComplex = {
    val r = this.re * that.re - this.im * that.im
    val i = this.re * that.im + this.im * that.re
    TestComplex(r, i)
  }

  def /(that: TestComplex): TestComplex = {
    val d = that.re * that.re + that.im * that.im
    val r = (this.re * that.re + this.im * that.im) / d
    val i = (this.im * that.re - this.re * that.im) / d
    TestComplex(r, i)
  }

  def *(x: Double): TestComplex = TestComplex(this.re * x, this.im * x)

  def /(x: Double): TestComplex = TestComplex(this.re / x, this.im / x)

}

object ComplexPerformance extends LocalTime {

  private val sizes: Gen[Int] = Gen.range("size")(200000, 1000000, 200000)

  private val ranges = for {
    size <- sizes
  } yield 0 until size

  private val z = Complex(3.0, 4.0)
  private var v = Complex.one
  performance of "Complex" in {
    measure method "multiply" in {
      using(ranges) in {
        _ foreach { _ =>
          v = z * v * Complex(0.2, 0.0)
        }
      }
    }
  }

  private var vre = 1.0
  private var vim = 0.0
  performance of "Complex" in {
    measure method "multiply comparison" in {
      using(ranges) in {
        _ foreach { _ =>
          val re = vre
          val im = vim
          vre = (3.0 * re - 4.0 * im) / 5.0
          vim = (3.0 * im + 4.0 * re) / 5.0
        }
      }
    }
  }

  private val tx = TestComplex(3.0, 4.0)
  private var ty = TestComplex(1.0, 0.0)
  performance of "Case class Complex" in {
    measure method "multiply comparison" in {
      using(ranges) in {
        _ foreach { _ =>
          ty = tx * ty * TestComplex(0.2, 0.0)
        }
      }
    }
  }

}
