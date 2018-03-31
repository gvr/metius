package cen.alpha.metius.complex

import org.scalatest.{Matchers, WordSpec}

class QuaternionSpec extends WordSpec with Matchers {

  "A Quaternion number" should {

    "be constructible from a real" in {
      Quaternion(1.2) shouldBe Quaternion(1.2, 0.0, 0.0, 0.0)
    }

    "have an absolute value (magnitude, norm)" in {
      Quaternion.zero.abs shouldBe 0.0
      Quaternion.one.abs shouldBe 1.0
      Quaternion.i.abs shouldBe 1.0
      Quaternion.j.abs shouldBe 1.0
      Quaternion.k.abs shouldBe 1.0
      Quaternion(+1.0, -1.0, +1.0, -1.0).abs shouldBe 2.0
      Quaternion(-1.0, -1.0, -1.0, +1.0).abs shouldBe 2.0
      Quaternion(0.0, 3.0, -4.0, 0.0).abs shouldBe 5.0
      Quaternion(0.0, 0.0, Double.NegativeInfinity, 0.0).abs shouldBe Double.PositiveInfinity
      Quaternion(0.0, Double.NaN, 0.0, 0.0).abs.isNaN shouldBe true
    }

    "have a size of the imaginary parts" in {
      Quaternion.zero.imag shouldBe 0.0
      Quaternion.one.imag shouldBe 0.0
      Quaternion.i.imag shouldBe 1.0
      Quaternion.j.imag shouldBe 1.0
      Quaternion.k.imag shouldBe 1.0
      Quaternion(1.2, -2.0, 1.0, 2.0).imag shouldBe 3.0
      Quaternion(1.0, 3.0, -4.0, 0.0).imag shouldBe 5.0
    }

    "have an argument as θ in q = r(cosθ + u sinθ)" in {
      Quaternion.zero.arg shouldBe 0.0
      Quaternion.one.arg shouldBe 0.0
      Quaternion.i.arg shouldBe math.Pi / 2
      Quaternion.j.arg shouldBe math.Pi / 2
      Quaternion.k.arg shouldBe math.Pi / 2
    }

    "have an inverse" in {
      Quaternion.one.inverse shouldBe Quaternion.one
      Quaternion.i.inverse shouldBe -Quaternion.i
      Quaternion.j.inverse shouldBe -Quaternion.j
      Quaternion.k.inverse shouldBe -Quaternion.k
      Quaternion(6.0, 8.0, 0.0, 0.0).inverse shouldBe Quaternion(0.06, -0.08, 0.0, 0.0)
      Quaternion(6.0, 0.0, 8.0, 0.0).inverse shouldBe Quaternion(0.06, 0.0, -0.08, 0.0)
      Quaternion(6.0, 0.0, 0.0, 8.0).inverse shouldBe Quaternion(0.06, 0.0, 0.0, -0.08)
    }

    "have a convenience square norm" in {
      Quaternion(1.0, 2.0, -3.0, 4.0).squareNorm shouldBe 30.0
    }

    "have a + operator" in {
      +Quaternion.zero shouldBe Quaternion.zero
      +Quaternion.one shouldBe Quaternion.one
    }

    "have a minus operation" in {
      -Quaternion.zero shouldBe Quaternion(0.0, 0.0, 0.0, 0.0)
      -Quaternion.one shouldBe Quaternion(-1.0, 0.0, 0.0, 0.0)
      -Quaternion.i shouldBe Quaternion(0.0, -1.0, 0.0, 0.0)
      -Quaternion.j shouldBe Quaternion(0.0, 0.0, -1.0, 0.0)
      -Quaternion.k shouldBe Quaternion(0.0, 0.0, 0.0, -1.0)

      val q = Quaternion(-1.1, 2.2, -3.3, 4.4)
      q.minus shouldBe Quaternion(1.1, -2.2, 3.3, -4.4)
      -q shouldBe Quaternion(1.1, -2.2, 3.3, -4.4)
      -(-q) shouldBe q
    }

    "have a conjugate operation" in {
      ~Quaternion.zero shouldBe Quaternion(0.0, 0.0, 0.0, 0.0)
      ~Quaternion.one shouldBe Quaternion(1.0, 0.0, 0.0, 0.0)
      ~Quaternion.i shouldBe Quaternion(0.0, -1.0, 0.0, 0.0)
      ~Quaternion.j shouldBe Quaternion(0.0, 0.0, -1.0, 0.0)
      ~Quaternion.k shouldBe Quaternion(0.0, 0.0, 0.0, -1.0)

      val q = Quaternion(-1.1, 2.2, -3.3, 4.4)
      q.conjugate shouldBe Quaternion(-1.1, -2.2, 3.3, -4.4)
      ~q shouldBe Quaternion(-1.1, -2.2, 3.3, -4.4)
      ~(~q) shouldBe q
    }

    "have a decent string representation" in {
      Quaternion.zero.toString shouldBe "0.0"
      Quaternion.one.toString shouldBe "1.0"
      Quaternion.i.toString shouldBe "1.0i"
      Quaternion.j.toString shouldBe "1.0j"
      Quaternion.k.toString shouldBe "1.0k"
      Quaternion(1.0, 2.0, 3.0, 4.0).toString shouldBe "1.0+2.0i+3.0j+4.0k"
      Quaternion(-1.0, -2.0, -3.0, -4.0).toString shouldBe "-1.0-2.0i-3.0j-4.0k"
      Quaternion(Double.NaN, Double.NaN, Double.NaN, Double.NaN).toString shouldBe "NaN+NaNi+NaNj+NaNk"
    }

    "add quaternion numbers" in {
      val a = Quaternion(1.0, 2.0, 3.0, 4.0)
      val b = Quaternion(0.1, 0.2, 0.3, 0.4)
      val c = Quaternion(1.1, 2.2, 3.3, 4.4)
      a + b shouldBe c
      b + a shouldBe c
      a + Quaternion.zero shouldBe a
      Quaternion.zero + a shouldBe a
    }

    "subtract quaternion numbers" in {
      val a = Quaternion(1.0, 2.0, 3.0, 4.0)
      val b = Quaternion(0.1, 0.2, 0.3, 0.4)
      val c = Quaternion(0.9, 1.8, 2.7, 3.6)
      a - b shouldBe c
      b - a shouldBe -c
      a - Quaternion.zero shouldBe a
      Quaternion.zero - a shouldBe -a
    }

    "multiply quaternion numbers" in {
      Quaternion.zero * Quaternion.zero shouldBe Quaternion.zero
      Quaternion.zero * Quaternion.one shouldBe Quaternion.zero
      Quaternion.one * Quaternion.one shouldBe Quaternion.one
      Quaternion.i * Quaternion.i shouldBe -Quaternion.one
      Quaternion.j * Quaternion.j shouldBe -Quaternion.one
      Quaternion.k * Quaternion.k shouldBe -Quaternion.one
      Quaternion.i * Quaternion.j * Quaternion.k shouldBe -Quaternion.one
      Quaternion.i * Quaternion.j shouldBe +Quaternion.k
      Quaternion.j * Quaternion.i shouldBe -Quaternion.k
      Quaternion.j * Quaternion.k shouldBe +Quaternion.i
      Quaternion.k * Quaternion.j shouldBe -Quaternion.i
      Quaternion.k * Quaternion.i shouldBe +Quaternion.j
      Quaternion.i * Quaternion.k shouldBe -Quaternion.j
      val a = Quaternion(1.0, 2.0, -3.0, 3.0)
      val b = Quaternion(2.0, -1.0, 6.0, -8.0)
      a * b shouldBe Quaternion(46.0, 9.0, 13.0, 7.0)
      b * a shouldBe Quaternion(46.0, -3.0, -13.0, -11.0)
      a * Quaternion.zero shouldBe Quaternion.zero
      Quaternion.zero * a shouldBe Quaternion.zero
      a * Quaternion.one shouldBe a
      Quaternion.one * a shouldBe a
    }

    "divide quaternion numbers" in {
      Quaternion.one / Quaternion.one shouldBe Quaternion.one
      Quaternion.one / Quaternion.i shouldBe -Quaternion.i
      Quaternion.one / Quaternion.j shouldBe -Quaternion.j
      Quaternion.one / Quaternion.k shouldBe -Quaternion.k
      Quaternion.i / Quaternion.j shouldBe -Quaternion.k
      val q = Quaternion(8.0, 6.0, 10.0, 2.0) / Quaternion(4.0, 3.0, 5.0, 1.0)
      q.real shouldBe 2.0 +- math.ulp(1.0)
      q.i shouldBe 0.0 +- math.ulp(1.0)
      q.j shouldBe 0.0 +- math.ulp(1.0)
      q.k shouldBe 0.0 +- math.ulp(1.0)
    }

    "add real numbers" in {
      Quaternion.zero + 1.0 shouldBe Quaternion.one
    }

    "subtract real numbers" in {
      Quaternion.zero - 1.0 shouldBe -Quaternion.one
    }

    "multiply real numbers" in {
      Quaternion(1.0, -2.0, 3.0, -4.0) * 2.5 shouldBe Quaternion(2.5, -5.0, 7.5, -10.0)
    }

    "divide real numbers" in {
      Quaternion(2.5, -5.0, 7.5, -10.0) / 2.5 shouldBe Quaternion(1.0, -2.0, 3.0, -4.0)
    }

    "have a square function" in {
      Quaternion.zero.square shouldBe Quaternion.zero
      Quaternion.one.square shouldBe Quaternion.one
      Quaternion.i.square shouldBe -Quaternion.one
      Quaternion.j.square shouldBe -Quaternion.one
      Quaternion.k.square shouldBe -Quaternion.one
      Quaternion(5.0, 4.0, -3.0, 2.0).square shouldBe Quaternion(-4.0, 40.0, -30.0, 20.0)
    }

    "have a sqruare root function" in {
      Quaternion.zero.sqrt shouldBe Quaternion.zero
      Quaternion.one.sqrt shouldBe Quaternion.one
      Quaternion(4.0).sqrt shouldBe Quaternion(2.0)
      Quaternion(-1.0).sqrt shouldBe Quaternion.i
      Quaternion(-4.0).sqrt shouldBe Quaternion.i * 2.0
      Quaternion.i.sqrt shouldBe Quaternion(math.sqrt(0.5), math.sqrt(0.5), 0.0, 0.0)
      Quaternion.j.sqrt shouldBe Quaternion(math.sqrt(0.5), 0.0, math.sqrt(0.5), 0.0)
      Quaternion.k.sqrt shouldBe Quaternion(math.sqrt(0.5), 0.0, 0.0, math.sqrt(0.5))
      Quaternion(0.0, 0.6, 0.0, -0.8).sqrt shouldBe
        Quaternion(math.sqrt(0.5), 0.6 * math.sqrt(0.5), 0.0, -0.8 * math.sqrt(0.5))
      val a = Quaternion(+3.0, 8.0, -4.0, 2.0)
      a.sqrt.square.real shouldBe 3.0 +- math.ulp(4.0)
      a.sqrt.square.i shouldBe 8.0
      a.sqrt.square.j shouldBe -4.0
      a.sqrt.square.k shouldBe 2.0
      val b = Quaternion(-3.0, 8.0, -4.0, 2.0)
      b.sqrt.square.real shouldBe -3.0 +- math.ulp(4.0)
      b.sqrt.square.i shouldBe 8.0
      b.sqrt.square.j shouldBe -4.0
      b.sqrt.square.k shouldBe 2.0
    }

    "have an exponential functon" in {
      Quaternion.zero.exp shouldBe Quaternion.one
      Quaternion.one.exp shouldBe Quaternion(math.exp(1.0))
      Quaternion.i.exp shouldBe Quaternion(math.cos(1.0), math.sin(1.0), 0.0, 0.0)
      Quaternion.j.exp shouldBe Quaternion(math.cos(1.0), 0.0, math.sin(1.0), 0.0)
      Quaternion.k.exp shouldBe Quaternion(math.cos(1.0), 0.0, 0.0, math.sin(1.0))
      Quaternion(1.0, 2.0, 0.0, 0.0).exp shouldBe
        Quaternion(math.exp(1.0) * math.cos(2.0), math.exp(1.0) * math.sin(2.0), 0.0, 0.0)
      Quaternion(0.0, 3.0, 0.0, -4.0).exp shouldBe
        Quaternion(math.cos(5.0), 0.6 * math.sin(5.0), 0.0, -0.8 * math.sin(5.0))
      Quaternion(0.0, -0.9, -1.2, 2.0).exp shouldBe
        Quaternion(math.cos(2.5), -0.36 * math.sin(2.5), -0.48 * math.sin(2.5), +0.80 * math.sin(2.5))
    }

    "have a log function" in {
      Quaternion.zero.log shouldBe Quaternion(Double.NegativeInfinity, 0.0, 0.0, 0.0)
      Quaternion.one.log shouldBe Quaternion.zero
      Quaternion.i.log shouldBe Quaternion(0.0, math.Pi / 2.0, 0.0, 0.0)
      Quaternion.j.log shouldBe Quaternion(0.0, 0.0, math.Pi / 2.0, 0.0)
      Quaternion.k.log shouldBe Quaternion(0.0, 0.0, 0.0, math.Pi / 2.0)
      val a = Quaternion(math.cos(2.5), -0.36 * math.sin(2.5), -0.48 * math.sin(2.5), +0.80 * math.sin(2.5)).log
      a.real shouldBe 0.0 +- math.ulp(1.0)
      a.i shouldBe -0.9
      a.j shouldBe -1.2
      a.k shouldBe 2.0
      val b = Quaternion(math.cos(1.0), 0.8 * math.sin(1.0), -0.6 * math.sin(1.0), 0.0).log
      b.real shouldBe 0.0
      b.i shouldBe 0.8 +- math.ulp(1.0)
      b.j shouldBe -0.6 +- math.ulp(1.0)
      b.k shouldBe 0.0
    }

  }

  "The Quaternion comanion object" should {

    "have quaternion argument functions like scala.math" in {
      Quaternion.abs(Quaternion.k) shouldBe 1.0
      Quaternion.square(Quaternion(5.0, 4.0, 3.0, 2.0)) shouldBe Quaternion(-4.0, 40.0, 30.0, 20.0)
      Quaternion.sqrt(Quaternion(4.0)) shouldBe Quaternion(2.0)
      Quaternion.exp(Quaternion.zero) shouldBe Quaternion.one
      Quaternion.log(Quaternion.one) shouldBe Quaternion.zero
    }

  }

}
