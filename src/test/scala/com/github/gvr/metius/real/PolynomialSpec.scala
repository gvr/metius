package com.github.gvr.metius.real

import org.scalatest.{Matchers, WordSpec}

class PolynomialSpec extends WordSpec with Matchers {

  "Polynomial" should {

    "have a ploynomial of order 0 (constant), just for completeness" in {
      Polynomial.apply(1.2)(1e99) shouldBe 1.2
      Polynomial(1.2)(-3.4) shouldBe 1.2
    }

    "have a polynomial of order 1 (linear)" in {
      Polynomial(1.0, 2.0)(3.0) shouldBe 7.0
      Polynomial(1.0, -2.0)(3.0) shouldBe -5.0
    }

    "have a polynomial of order 2" in {
      Polynomial(1.0, 0.0, 2.0)(3.0) shouldBe 19.0
      Polynomial(1.0, 1.5, -2.0)(3.0) shouldBe -12.5
    }

    "have a polynomial of order 3" in {
      val p = Polynomial(4.0, 3.0, 2.0, 1.0)
      p(-2.0) shouldBe -2.0
      p(-1.0) shouldBe +2.0
      p(+0.0) shouldBe +4.0
      p(+1.0) shouldBe +10.0
      p(+2.0) shouldBe +26.0
    }

    "have a polynomial of order 4" in {
      val p = Polynomial(5.0, 4.0, 3.0, 2.0, 1.0)
      p(-2.0) shouldBe +9.0
      p(-1.0) shouldBe +3.0
      p(+0.0) shouldBe +5.0
      p(+1.0) shouldBe +15.0
      p(+2.0) shouldBe +57.0
    }

    "have a polynomial of order 5" in {
      val p = Polynomial(6.0, 5.0, 4.0, 3.0, 2.0, 1.0)
      p(-2.0) shouldBe -12.0
      p(-1.0) shouldBe +3.0
      p(+0.0) shouldBe +6.0
      p(+1.0) shouldBe +21.0
      p(+2.0) shouldBe +120.0
    }

    "have a polynomial of order 6" in {
      val p = Polynomial(7.0, 6.0, 5.0, 4.0, 3.0, 2.0, 1.0)
      p(-2.0) shouldBe +31.0
      p(-1.0) shouldBe +4.0
      p(+0.0) shouldBe +7.0
      p(+1.0) shouldBe +28.0
      p(+2.0) shouldBe +247.0
    }

    "have a polynomial of order 7" in {
      val p = Polynomial(8.0, 7.0, 6.0, 5.0, 4.0, 3.0, 2.0, 1.0)
      p(-2.0) shouldBe -54.0
      p(-1.0) shouldBe +4.0
      p(+0.0) shouldBe +8.0
      p(+1.0) shouldBe +36.0
      p(+2.0) shouldBe +502.0
    }

    "have a polynomial of order 8" in {
      val p = Polynomial(9.0, 8.0, 7.0, 6.0, 5.0, 4.0, 3.0, 2.0, 1.0)
      p(-2.0) shouldBe +117.0
      p(-1.0) shouldBe +5.0
      p(+0.0) shouldBe +9.0
      p(+1.0) shouldBe +45.0
      p(+2.0) shouldBe +1013.0
    }

    "have a polynomial of order 9" in {
      val p = Polynomial(10.0, 9.0, 8.0, 7.0, 6.0, 5.0, 4.0, 3.0, 2.0, 1.0)
      p(-2.0) shouldBe -224.0
      p(-1.0) shouldBe +5.0
      p(+0.0) shouldBe +10.0
      p(+1.0) shouldBe +55.0
      p(+2.0) shouldBe +2036.0
    }

    "have a polynomial of order 10" in {
      val p = Polynomial(11.0, 10.0, 9.0, 8.0, 7.0, 6.0, 5.0, 4.0, 3.0, 2.0, 1.0)
      p(-2.0) shouldBe +459.0
      p(-1.0) shouldBe +6.0
      p(+0.0) shouldBe +11.0
      p(+1.0) shouldBe +66.0
      p(+2.0) shouldBe +4083.0
    }

    "have a polynomial of order 11" in {
      val p = Polynomial(12.0, 11.0, 10.0, 9.0, 8.0, 7.0, 6.0, 5.0, 4.0, 3.0, 2.0, 1.0)
      p(-2.0) shouldBe -906.0
      p(-1.0) shouldBe +6.0
      p(+0.0) shouldBe +12.0
      p(+1.0) shouldBe +78.0
      p(+2.0) shouldBe +8178.0
    }

    "have a polynomial of order 12" in {
      val p = Polynomial(13.0, 12.0, 11.0, 10.0, 9.0, 8.0, 7.0, 6.0, 5.0, 4.0, 3.0, 2.0, 1.0)
      p(-2.0) shouldBe +1825.0
      p(-1.0) shouldBe +7.0
      p(+0.0) shouldBe +13.0
      p(+1.0) shouldBe +91.0
      p(+2.0) shouldBe +16369.0
    }

    "have a polynomial of order 13" in {
      val p = Polynomial(14.0, 13.0, 12.0, 11.0, 10.0, 9.0, 8.0, 7.0, 6.0, 5.0, 4.0, 3.0, 2.0, 1.0)
      p(-2.0) shouldBe -3636.0
      p(-1.0) shouldBe +7.0
      p(+0.0) shouldBe +14.0
      p(+1.0) shouldBe +105.0
      p(+2.0) shouldBe +32752.0
    }

    "have a polynomial of order 14" in {
      val p = Polynomial(15.0, 14.0, 13.0, 12.0, 11.0, 10.0, 9.0, 8.0, 7.0, 6.0, 5.0, 4.0, 3.0, 2.0, 1.0)
      p(-2.0) shouldBe +7287.0
      p(-1.0) shouldBe +8.0
      p(+0.0) shouldBe +15.0
      p(+1.0) shouldBe +120.0
      p(+2.0) shouldBe +65519.0
    }

    "have a polynomial of order 15" in {
      val p = Polynomial(16.0, 15.0, 14.0, 13.0, 12.0, 11.0, 10.0, 9.0, 8.0, 7.0, 6.0, 5.0, 4.0, 3.0, 2.0, 1.0)
      p(-2.0) shouldBe -14558.0
      p(-1.0) shouldBe +8.0
      p(+0.0) shouldBe +16.0
      p(+1.0) shouldBe +136.0
      p(+2.0) shouldBe +131054.0
    }

    "have a polynomial of arbitrary order" in {
      val p = Polynomial(17.0, 16.0, 15.0, 14.0, 13.0, 12.0, 11.0, 10.0, 9.0, 8.0, 7.0, 6.0, 5.0, 4.0, 3.0, 2.0, 1.0, 0.0)
      p(-2.0) shouldBe +29133.0
      p(-1.0) shouldBe +9.0
      p(+0.0) shouldBe +17.0
      p(+1.0) shouldBe +153.0
      p(+2.0) shouldBe +262125.0
    }

    "evaluate via horner's scheme" in {
      Polynomial.horner(Seq())(1.0) shouldBe 0.0
      Polynomial.horner(Seq(3.0, 5.0, 7.0))(2.0) shouldBe (3 + 5 * 2 + 7 * 2 * 2)
      Polynomial.horner(List(13.0, 5.0, 7.0, 11.0))(3.0) shouldBe (13 + 5 * 3 + 7 * 3 * 3 + 11 * 3 * 3 * 3)
      Polynomial.horner(Array(13.0, 5.0, 7.0, 11.0))(3.0) shouldBe (13 + 5 * 3 + 7 * 3 * 3 + 11 * 3 * 3 * 3)
    }

  }

}
