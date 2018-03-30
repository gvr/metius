package cen.alpha.metius.complex

import org.scalatest.{Matchers, WordSpec}

import scala.math.pow

class ComplexSpec extends WordSpec with Matchers {

  "A Complex number" should {

    "be constructible from a real and imaginary part" in {
      val z = Complex(3.3, -4.4)
      z.real shouldBe 3.3
      z.imag shouldBe -4.4
    }

    "be constructible from a real part" in {
      val z = Complex(3.3)
      z.real shouldBe 3.3
      z.imag shouldBe 0.0
      z shouldBe Complex(3.3, 0.0)
    }

    "be constructible from polar coordinates" in {
      Complex.polar(2.0, -math.Pi / 6).real shouldBe math.sqrt(3.0) +- math.ulp(1.0)
      Complex.polar(2.0, -math.Pi / 6).imag shouldBe -1.0 +- math.ulp(1.0)
      Complex.polar(2.5, 0.0) shouldBe Complex(2.5, 0.0)
      Complex.polar(2.5, math.Pi / 2).imag shouldBe 2.5
      Complex.polar(2.5, math.Pi / 2).real shouldBe 0.0 +- math.ulp(1.0)
    }

    "have an unapply method as case class" in {
      Complex(2.0, 3.0) match {
        case Complex(x, y) =>
          x shouldBe 2.0
          y shouldBe 3.0
        case _ => true shouldBe false
      }
    }

    "have a copy constructor as case class" in {
      Complex(2.0, 3.0).copy(real = 4.0) shouldBe Complex(4.0, 3.0)
      Complex(2.0, 3.0).copy(imag = 4.0) shouldBe Complex(2.0, 4.0)
      Complex(2.0, 3.0).copy(real = 4.0, imag = 4.1) shouldBe Complex(4.0, 4.1)
    }

    "have a real component" in {
      Complex(2.0, 3.0).real shouldBe 2.0
    }

    "have an imaginary component" in {
      Complex(2.0, 3.0).imag shouldBe 3.0
    }

    "have a modulus" in {
      Complex.zero.abs shouldBe 0.0
      Complex.i.abs shouldBe 1.0
      Complex.one.abs shouldBe 1.0
      Complex(+3.3, 0.0).abs shouldBe 3.3
      Complex(-3.3, 0.0).abs shouldBe 3.3
      Complex(0.0, +4.4).abs shouldBe 4.4
      Complex(0.0, -4.4).abs shouldBe 4.4
      Complex(+3.3, +4.4).abs shouldBe 5.5
      Complex(+3.3, -4.4).abs shouldBe 5.5
      Complex(-3.3, +4.4).abs shouldBe 5.5
      Complex(-3.3, -4.4).abs shouldBe 5.5
    }

    "have an argument" in {
      Complex.zero.arg shouldBe 0.0
      Complex.one.arg shouldBe 0.0
      Complex.i.arg shouldBe +1.57079632679489661923132
      Complex(-1.0, 0.0).arg shouldBe +3.14159265358979323846264
      Complex(+0.0, -1.0).arg shouldBe -1.57079632679489661923132
      Complex(+2.5, +2.5).arg shouldBe +0.785398163397448309615661
      Complex(+2.5, -2.5).arg shouldBe -0.785398163397448309615661
      Complex(-2.5, +2.5).arg shouldBe +2.35619449019234492884698
      Complex(-2.5, -2.5).arg shouldBe -2.35619449019234492884698
      Complex(+7.5, +5.0).arg shouldBe +0.588002603547567551245611
      Complex(+7.5, -5.0).arg shouldBe -0.588002603547567551245611
      Complex(-7.5, +5.0).arg shouldBe +2.55359005004222568721703
      Complex(-7.5, -5.0).arg shouldBe -2.55359005004222568721703
      Complex(+5.0, +7.5).arg shouldBe +0.982793723247329067985711
    }

    "have a decent toString method" in {
      Complex(0.0, 0.0).toString shouldBe "0.0"
      Complex(+3.3, 0.0).toString shouldBe "3.3"
      Complex(-3.3, 0.0).toString shouldBe "-3.3"
      Complex(0.0, +4.4).toString shouldBe "4.4i"
      Complex(0.0, -4.4).toString shouldBe "-4.4i"
      Complex(+3.3, +4.4).toString shouldBe "3.3+4.4i"
      Complex(+3.3, -4.4).toString shouldBe "3.3-4.4i"
      Complex(-3.3, +4.4).toString shouldBe "-3.3+4.4i"
      Complex(-3.3, -4.4).toString shouldBe "-3.3-4.4i"
    }

    "have predefined constants" in {
      Complex.zero.real shouldBe 0.0
      Complex.zero.imag shouldBe 0.0
      Complex.one.real shouldBe 1.0
      Complex.one.imag shouldBe 0.0
      Complex.i.real shouldBe 0.0
      Complex.i.imag shouldBe 1.0
    }

    "have a + operator" in {
      +Complex.zero shouldBe Complex.zero
      +Complex.one shouldBe Complex.one
    }

    "have a unary minus operation" in {
      val z = Complex(3.3, 4.4)
      z.minus shouldBe Complex(-3.3, -4.4)
      -z shouldBe Complex(-3.3, -4.4)
      -(-z) shouldBe z
    }

    "have a conjugate as unary operation" in {
      val z = Complex(3.3, 4.4)
      z.conjugate shouldBe Complex(3.3, -4.4)
      ~z shouldBe Complex(3.3, -4.4)
      ~(~z) shouldBe z
    }

    "have a mirror operation (real -> -real)" in {
      val z = Complex(3.3, 4.4)
      z.mirror shouldBe Complex(-3.3, 4.4)
      z.mirror.mirror shouldBe z
      z.mirror shouldBe ~(-z)
    }

    "have an inverse" in {
      Complex.one.inverse shouldBe Complex.one
      Complex.i.inverse shouldBe -Complex.i
      Complex(3.0, 4.0).inverse shouldBe Complex(0.12, -0.16)
      Complex(4.0, -3.0).inverse shouldBe Complex(0.16, 0.12)
      Complex(0.0, Double.PositiveInfinity).inverse shouldBe Complex.zero
      Complex(0.0, exp2(1023)).inverse shouldBe Complex(0.0, -exp2(-1023))
      Complex(exp2(1020), exp2(-1020)).inverse shouldBe Complex(exp2(-1020), 0.0)
      Complex(exp2(-1020), exp2(1020)).inverse shouldBe Complex(0.0, -exp2(-1020))
    }

    "have an equals method (from case class; test serves behavior reference)" in {
      val z = Complex(3.3, 4.4)
      (z == Complex(3.3, 4.4)) shouldBe true
      (z == Complex(3.3, 4.5)) shouldBe false
      (z != Complex(3.3, 4.5)) shouldBe true
      (Complex(Double.PositiveInfinity, 0.0) == Complex(Double.NegativeInfinity, 0.0)) shouldBe false
      (Complex(Double.PositiveInfinity, 0.0) == Complex(3.3, Double.NegativeInfinity)) shouldBe false
      (Complex(Double.NaN, 0.0) == Complex(Double.NaN, 0.0)) shouldBe false
      Complex(Double.NaN).eq(Complex(Double.NaN, 0.0)) shouldBe false
      Complex(Double.NaN).equals(Complex(Double.NaN, 0.0)) shouldBe false
      (Complex(Double.NaN, 0.0) == Complex(3.3, Double.NaN)) shouldBe false

      // a side note for fun / amazement:
      assert((Double.NaN == Double.NaN) == false)
      assert(Double.NaN.equals(Double.NaN) == true)
    }

    "have a square norm convenience method" in {
      Complex(3.0, -4.0).squareNorm shouldBe 25.0
    }

    "add complex numbers" in {
      val a = Complex(5.5, 3.5)
      val b = Complex(3.5, 2.5)
      a + b shouldBe Complex(9.0, 6.0)
      b + a shouldBe Complex(9.0, 6.0)
      a + Complex.zero shouldBe a
      Complex.zero + a shouldBe a
    }

    "subtract complex numbers" in {
      val a = Complex(5.5, 3.5)
      val b = Complex(3.5, 2.5)
      a - b shouldBe Complex(2.0, 1.0)
      b - a shouldBe Complex(-2.0, -1.0)
      a - Complex.zero shouldBe a
      Complex.zero - a shouldBe -a
    }

    "multiply complex numbers" in {
      val a = Complex(5.5, 3.5)
      val b = Complex(3.5, 2.5)
      a * b shouldBe Complex(10.5, 26.0)
      b * a shouldBe Complex(10.5, 26.0)
      a * Complex.one shouldBe a
      Complex.one * a shouldBe a
      -Complex.one * a shouldBe -a
    }

    "divide complex numbers" in {
      Complex(2, 1) / Complex(1, 0.5) shouldBe Complex(2, 0)
      Complex(8.0, 6.0) / Complex(3.0, 4.0) shouldBe Complex(1.92, -0.56)
      Complex(3.0, 4.0) / Complex(8.0, 6.0) shouldBe Complex(0.48, 0.14)
      Complex(8.0, 6.0) / Complex.one shouldBe Complex(8.0, 6.0)
      Complex(8.0, 6.0) / Complex.i shouldBe Complex(6.0, -8.0)
      Complex.one / Complex(8.0, 6.0) shouldBe Complex(0.08, -0.06)
      Complex.one / Complex.i shouldBe -Complex.i
      Complex(Double.MaxValue, 0.0) / Complex(Double.MaxValue, 0.0) shouldBe Complex(1.0, 0.0)
      Complex(0.0, Double.MaxValue) / Complex(0.0, Double.MaxValue) shouldBe Complex(1.0, 0.0)
      (Complex(Double.PositiveInfinity, 1.0) / Complex(Double.PositiveInfinity, 1.0)).real.isNaN shouldBe true
      (Complex(1.0, Double.PositiveInfinity) / Complex(Double.PositiveInfinity, 1.0)).imag.isNaN shouldBe true
      Complex.i / Complex(Double.PositiveInfinity, 0.0) shouldBe Complex.zero
      Complex.i / Complex(0.0, Double.PositiveInfinity) shouldBe Complex.zero

      // some edge cases from Baudin & Smith (2012) "A Robust Complex Division in Scilab" (arXiv:1210.4539v2)
      Complex(1, 1) / Complex(1, exp2(1023)) shouldBe Complex(exp2(-1023), -exp2(-1023))
      Complex(1, 1) / Complex(exp2(-1023), exp2(-1023)) shouldBe Complex(exp2(1023), 0.0)
      Complex(exp2(1023), exp2(-1023)) / Complex(exp2(677), exp2(-677)) shouldBe Complex(exp2(346), -exp2(-1008))
      //Complex(exp2(1023), exp2(1023)) / Complex(1, 1) shouldBe Complex(exp2(1023), 0.0)
      Complex(exp2(1020), exp2(-844)) / Complex(exp2(656), exp2(-780)) shouldBe Complex(exp2(364), -exp2(-1072))
      Complex(exp2(-71), exp2(1021)) / Complex(exp2(1001), exp2(-323)) shouldBe Complex(exp2(-1072), exp2(20))
      //Complex(exp2(-347), exp2(-54)) / Complex(exp2(-1037), exp2(-1058)) shouldBe Complex(3.898125604559113300e289, 8.174961907852353577e295)
      //Complex(exp2(-1074), exp2(-1074)) / Complex(exp2(-1073), exp2(-1074)) shouldBe Complex(0.6, 0.2)
      //Complex(exp2(1015), exp2(-989)) / Complex(exp2(1023), exp2(1023)) shouldBe Complex(0.001953125, -0.001953125)
      Complex(exp2(-622), exp2(-1071)) / Complex(exp2(-343), exp2(-798)) shouldBe Complex(1.02951151789360578e-84, 6.97145987515076231e-220)

      // less on the edge
      Complex(exp2(1022), exp2(1022)) / Complex(1, 1) shouldBe Complex(exp2(1022), 0.0)
      Complex(exp2(1015), exp2(-989)) / Complex(exp2(1022), exp2(1022)) shouldBe Complex(0.001953125, -0.001953125) * 2.0
    }

    "add real numbers" in {
      val z = Complex(6.0, 3.0)
      val x = 1.5
      Complex.zero + x shouldBe Complex(x, 0.0)
      z + x shouldBe Complex(7.5, 3.0)
      //x + z shouldBe Complex(7.5, 3.0)
    }

    "subtract real numbers" in {
      val z = Complex(6.0, 3.0)
      val x = 1.5
      Complex.zero - x shouldBe Complex(-x, 0.0)
      z - x shouldBe Complex(4.5, 3.0)
    }

    "multiply real numbers" in {
      val z = Complex(6.0, 3.0)
      val x = 1.5
      Complex.zero * x shouldBe Complex.zero
      Complex.one * x shouldBe Complex(x, 0.0)
      z * x shouldBe Complex(9.0, 4.5)
    }

    "divide real numbers" in {
      val z = Complex(6.0, 3.0)
      val x = 1.5
      Complex.zero / x shouldBe Complex.zero
      Complex.one / x shouldBe Complex(1.0 / x, 0.0)
      z / x shouldBe Complex(4.0, 2.0)
    }

    "have a square" in {
      Complex.zero.square shouldBe Complex.zero
      Complex.one.square shouldBe Complex.one
      Complex.i.square shouldBe -Complex.one

      Complex(+2.0, 0.0).square shouldBe Complex(+4.0, 0.0)
      Complex(-2.0, 0.0).square shouldBe Complex(+4.0, 0.0)
      Complex(0.0, +2.0).square shouldBe Complex(-4.0, 0.0)
      Complex(0.0, -2.0).square shouldBe Complex(-4.0, 0.0)
      Complex(+1.0, +1.0).square shouldBe Complex(0.0, +2.0)
      Complex(-1.0, +1.0).square shouldBe Complex(0.0, -2.0)
      Complex(+1.0, -1.0).square shouldBe Complex(0.0, -2.0)
      Complex(-1.0, -1.0).square shouldBe Complex(0.0, +2.0)
    }

    "have a square root" in {
      // the square root is choosen to always have a non-negative real
      Complex.zero.sqrt shouldBe Complex.zero
      Complex.one.sqrt shouldBe Complex.one
      Complex.i.sqrt shouldBe Complex(math.sqrt(0.5), math.sqrt(0.5))
      Complex(+4.0, 0.0).sqrt shouldBe Complex(2.0, 0.0)
      Complex(-4.0, 0.0).sqrt shouldBe Complex(0.0, 2.0)
      Complex(0.0, +8.0).sqrt shouldBe Complex(+2.0, +2.0)
      Complex(0.0, -8.0).sqrt shouldBe Complex(+2.0, -2.0)
      Complex.polar(4.0, +1.5).sqrt shouldBe Complex.polar(2.0, +0.75)
      Complex.polar(4.0, -1.5).sqrt shouldBe Complex.polar(2.0, -0.75)
      Complex.polar(4.0, +2.0).sqrt shouldBe Complex.polar(2.0, +1.0)
      Complex.polar(4.0, -2.0).sqrt shouldBe Complex.polar(2.0, -1.0)
    }

    "have an exponential function" in {
      val z = Complex(2.0, math.Pi / 6).exp
      z.real shouldBe math.exp(2.0) * math.cos(math.Pi / 6)
      z.imag shouldBe math.exp(2.0) * math.sin(math.Pi / 6)
      Complex.zero.exp shouldBe Complex.one
      Complex.i.exp.real shouldBe math.cos(1.0)
      Complex.i.exp.imag shouldBe math.sin(1.0)
    }

    "have a logarithm function" in {
      Complex.polar(math.exp(2.0), 1.23).log shouldBe Complex(2.0, 1.23)
      Complex.polar(1.0, 1.23).log shouldBe Complex(0.0, 1.23)
      Complex.one.log shouldBe Complex.zero
    }

  }

  "The Complex companion object" should {

    "have complex argument functions like scala.math for complex numbers" in {
      Complex.abs(Complex.i) shouldBe 1.0
      Complex.square(Complex.i) shouldBe -Complex.one
      Complex.sqrt(Complex(-4.0, 0.0)) shouldBe Complex(0.0, 2.0)
      Complex.exp(Complex.zero) shouldBe Complex.one
      Complex.log(Complex.one) shouldBe Complex.zero
    }

  }

  def exp2(x: Int): Double = pow(2.0, x.toDouble)

}
