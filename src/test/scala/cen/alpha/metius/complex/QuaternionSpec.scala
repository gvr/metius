package cen.alpha.metius.complex

import org.scalatest.{Assertion, Matchers, WordSpec}

class QuaternionSpec extends WordSpec with Matchers {

  "A Quaternion number" should {

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

  }

  "The Quaternion comanion object" should {

    "have quaternion argument functions like scala.math" in {
      Quaternion.abs(Quaternion.k) shouldBe 1.0
    }

  }

}
