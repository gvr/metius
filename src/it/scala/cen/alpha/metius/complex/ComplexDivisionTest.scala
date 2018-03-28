package cen.alpha.metius.complex

import org.scalatest.FunSuite

import scala.math.{abs, hypot, pow}

class ComplexDivisionTest extends FunSuite {

  case class BinaryOperatorResult(x: Complex, y: Complex, expected: Complex)

  def exp2(x: Int): Double = pow(2.0, x.toDouble)

  private def simpleDiv(x: Complex, y: Complex): Complex = {
    val denominator = y.real * y.real + y.imag * y.imag
    val real = (x.real * y.real + x.imag * y.imag) / denominator
    val imaginary = (x.imag * y.real - x.real * y.imag) / denominator
    Complex(real, imaginary)
  }

  private def testDiv(x: Complex, y: Complex): Complex = {
    val s = math.hypot(y.real, y.imag)
    val a = x.real / s
    val b = x.imag / s
    val c = y.real / s
    val d = y.imag / s
    val real = (a * c + b * d)
    val imaginary = (b * c - a * d)
    Complex(real, imaginary)
  }

  private def improvedDiv(x: Complex, y: Complex): Complex = {
    val a = x.real
    val b = x.imag
    val c = y.real
    val d = y.imag
    if (abs(c) >= abs(d)) improvedDivInner(a, b, c, d)
    else improvedDivInner(-b, a, -d, c)
  }

  private def improvedDivInner(a: Double, b: Double, c: Double, d: Double): Complex = {
    if (c == 0.0) {
      if (a != 0.0 || b != 0.0) Complex.infinity
      else Complex.NaN
    }
    else if (c.isInfinite) {
      if (a.isInfinite || b.isInfinite) Complex.NaN
      else Complex.zero
    }
    else if (d == 0.0) {
      Complex(a / c, b / c)
    }
    else {
      val r = d / c
      if (r != 0.0) {
        val t = 1.0 / (c + d * r)
        val re = (a + b * r) * t
        val im = (b - a * r) * t
        Complex(re, im)
      }
      else {
        val t = 1.0 / c
        val re = (a + d * (b * t)) * t
        val im = (b - d * (a * t)) * t
        Complex(re, im)
      }
    }
  }

  private def improvedDiv2(x: Complex, y: Complex): Complex = {
    val a = x.real
    val b = x.imag
    val c = y.real
    val d = y.imag
    if (abs(d) <= abs(c)) improvedDivInner2(a, b, c, d)
    else improvedDivInner2(-b, a, -d, c)
  }

  private def improvedDivInner2(a: Double, b: Double, c: Double, d: Double): Complex = {
    val r = d / c
    if (r != 0.0) {
      val t = 1.0 / (c + d * r)
      val br = b * r
      val re =
        if (br != 0.0) (a + br) * t
        else a * t + (b * t) * r
      val ar = a * r
      val im =
        if (ar != 0.0) (b - ar) * t
        else b * t - (a * t) * r
      Complex(re, im)
    }
    else {
      val t = 1.0 / c
      val re = (a + d * (b * t)) * t
      val im = (b - d * (a * t)) * t
      Complex(re, im)
    }
  }

  // Edge test cases from M. Baudin & R.L. Smith (2012) A Robust Complex Division in Scilab (arXiv:1210.4539v2)
  private val z7 = Complex(3.898125604559113300e289, 8.174961907852353577e295)
  private val z9 = Complex(0.001953125, -0.001953125)
  private val z10 = Complex(1.02951151789360578e-84, 6.97145987515076231e-220)

  val divisionResults = List(
    BinaryOperatorResult(Complex(2, 1), Complex(1, 0.5), Complex(2, 0)),
    // see: Baudin & Smith (2012) "A Robust Complex Division in Scilab" (arXiv:1210.4539v2)
    BinaryOperatorResult(Complex(1, 1), Complex(1, exp2(1023)), Complex(exp2(-1023), -exp2(-1023))),
    BinaryOperatorResult(Complex(1, 1), Complex(exp2(-1023), exp2(-1023)), Complex(exp2(1023), 0.0)),
    BinaryOperatorResult(Complex(exp2(1023), exp2(-1023)), Complex(exp2(677), exp2(-677)), Complex(exp2(346), -exp2(-1008))),
    BinaryOperatorResult(Complex(exp2(1023), exp2(1023)), Complex(1, 1), Complex(exp2(1023), 0.0)),
    BinaryOperatorResult(Complex(exp2(1020), exp2(-844)),   Complex(exp2(656), exp2(-780)), Complex(exp2(364), -exp2(-1072))),
    BinaryOperatorResult(Complex(exp2(-71), exp2(1021)),    Complex(exp2(1001), exp2(-323)), Complex(exp2(-1072), exp2(20))),
    BinaryOperatorResult(Complex(exp2(-347), exp2(-54)),    Complex(exp2(-1037), exp2(-1058)), z7),
    BinaryOperatorResult(Complex(exp2(-1074), exp2(-1074)), Complex(exp2(-1073), exp2(-1074)), Complex(0.6, 0.2)),
    BinaryOperatorResult(Complex(exp2(1015), exp2(-989)),   Complex(exp2(1023), exp2(1023)), z9),
    BinaryOperatorResult(Complex(exp2(-622), exp2(-1071)),  Complex(exp2(-343), exp2(-798)), z10)
  )

  def check(result: BinaryOperatorResult, operator: (Complex, Complex) => Complex): Unit = {
    val actual = operator(result.x, result.y)
    val dx = result.expected.real - actual.real
    val dy = result.expected.imag - actual.imag
    val diff = hypot(dx, dy)
    println(s"x=${result.x} y=${result.y} expected=${result.expected} actual=$actual diff=$diff")
  }

  def check(results: List[BinaryOperatorResult], operator: (Complex, Complex) => Complex): Unit =
    results foreach { result => check(result, operator) }

  test("test division") {
    println()
    println("NAIVE")
    check(divisionResults, simpleDiv _)
  }

  test("naive division") {
    println()
    println("TEST")
    check(divisionResults, testDiv _)
  }

  test("improved division") {
    println()
    println("IMPROVED")
    check(divisionResults, improvedDiv _)
  }

  test("improved division 2") {
    println()
    println("IMPROVED2")
    check(divisionResults, improvedDiv2 _)
  }

}
