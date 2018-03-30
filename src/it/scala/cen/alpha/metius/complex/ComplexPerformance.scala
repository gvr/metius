package cen.alpha.metius.complex

import org.scalameter.Bench.LocalTime
import org.scalameter.Gen

object ComplexPerformance extends LocalTime {

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

  val sizes = Gen.range("size")(1000000, 80000000, 10000000)

  val ranges = for {
    size <- sizes
  } yield 0 until size

  val z = Complex(3.0, 4.0)
  var v = Complex.one

  performance of "Complex" in {
    measure method "multiply" in {
      using(ranges) in {
        _ => v = z * v * Complex(0.2, 0.0)
      }
    }
  }

  var vre = 1.0
  var vim = 0.0
  performance of "Complex" in {
    measure method "multiply comparison" in {
      using(ranges) in {
        _ => {
          val re = vre
          val im = vim
          vre = (3.0 * re - 4.0 * im) / 5.0
          vim = (3.0 * im + 4.0 * re) / 5.0
        }
      }
    }
  }

  var tre = TestComplex(1.0, 0.0)
  var tim = TestComplex(0.0, 0.0)
  performance of "Case class Complex" in {
    measure method "multiply comparison" in {
      using(ranges) in {
        _ => {
          val re = tre
          val im = tim
          tre = (re * 0.6) - (im * 0.8)
          tim = (im * 0.6) + (re * 0.8)
        }
      }
    }
  }

//  val sz = SpireComplex(3.0, 4.0)
//  var sv = SpireComplex(1.0)
//  performance of "Spire Complex" in {
//    measure method "multiply" in {
//      using(ranges) in {
//        _ => sv = sz * sv * 0.2
//      }
//    }
//  }



  println(v)
  println(vre)
  println(vim)
  println(tre)
  println(tim)
//  println(sv)

}
