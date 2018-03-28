package cen.alpha.metius.complex

import scala.math.{abs, atan2, hypot}

final case class Complex(real: Double, imag: Double) {

  def isNaN: Boolean = real.isNaN || imag.isNaN

  private def isInfiniteOrNaN: Boolean = real.isInfinite || imag.isInfinite

  def isInfinite: Boolean = (real.isInfinite && !imag.isNaN) || (imag.isInfinite && !real.isNaN)

  def abs: Double = hypot(real, imag)

  def arg: Double = atan2(imag, real)

  def squareNorm: Double = real * real + imag * imag

  def minus: Complex = Complex(-real, -imag)

  def conjugate: Complex = Complex(real, -imag)

  def mirror: Complex = Complex(-real, imag)

  def unary_- : Complex = minus

  def unary_~ : Complex = conjugate

  def inverse: Complex = Complex.inverse(this.real, this.imag)

  def +(other: Complex): Complex = {
    val re = this.real + other.real
    val im = this.imag + other.imag
    Complex(re, im)
  }

  def -(other: Complex): Complex = {
    val re = this.real - other.real
    val im = this.imag - other.imag
    Complex(re, im)
  }

  def *(other: Complex): Complex = {
    val re = this.real * other.real - this.imag * other.imag
    val im = this.real * other.imag + this.imag * other.real
    Complex(re, im)
  }

  def /(other: Complex): Complex = {
    Complex.divide(this.real, this.imag, other.real, other.imag)
  }

  def +(x: Double): Complex = Complex(this.real + x, this.imag)

  def -(x: Double): Complex = Complex(this.real - x, this.imag)

  def *(x: Double): Complex = Complex(this.real * x, this.imag * x)

  def /(x: Double): Complex = Complex(this.real / x, this.imag / x)

  // NOTE that NaN does not equal NaN for Double (using ==) but does here
  // NOTE2 for Double.NaN equals yields true for anothe NaN, but == yields false...
  override def equals(that: Any): Boolean = {
    that.isInstanceOf[Complex] && {
      val c = that.asInstanceOf[Complex]
      (real == c.real && imag == c.imag) ||
        (isNaN && c.isNaN) ||
        (isInfinite && c.isInfinite)
    }
  }

  override def hashCode: Int = {
    if (isNaN) Double.NaN.hashCode
    else if (isInfiniteOrNaN) Double.PositiveInfinity.hashCode
    else real.hashCode + 31 * imag.hashCode
  }

  private def imagToString: String = s"${imag.toString}i"

  override def toString: String = {
    if (isNaN) "NaN"
    else if (isInfiniteOrNaN) "Infinity"
    else if (imag == 0.0) real.toString
    else if (real == 0.0) imagToString
    else s"${real.toString}${if (imag > 0) "+" else ""}${imagToString}"
  }

}

object Complex {

  val zero = Complex(0.0, 0.0)

  val one = Complex(1.0, 0.0)

  val i = Complex(0.0, 1.0)

  val infinity = Complex(Double.PositiveInfinity, 0.0)

  val NaN = Complex(Double.NaN, Double.NaN)

  def apply(real: Double): Complex = Complex(real, 0.0)

  private def inverse(c: Double, d: Double): Complex = {
    if (d == 0.0) {
      if (c != 0.0) Complex(1.0 / c, 0.0)
      else Complex.infinity
    }
    else if (c.isInfinite || d.isInfinite) Complex.zero
    else if (abs(c) >= abs(d)) {
      val r = d / c
      if (r != 0.0) {
        val re = 1.0 / (c + d * r)
        val im = -r * re
        Complex(re, im)
      }
      else {
        val re = 1.0 / c
        val im = d * re * re
        Complex(re, im)
      }
    }
    else {
      val r = c / d
      if (r != 0.0) {
        val im = -1.0 / (d + c * r)
        val re = -r * im
        Complex(re, im)
      }
      else {
        val im = -1.0 / d
        val re = c * im * im
        Complex(re, im)
      }

    }
  }

  private def divide(a: Double, b: Double, c: Double, d: Double): Complex = {
    if (abs(c) >= abs(d)) divideOrdered(a, b, c, d)
    else divideOrdered(-b, a, -d, c)
  }

  private def divideOrdered(a: Double, b: Double, c: Double, d: Double): Complex = {
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
  }

}