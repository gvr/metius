package cen.alpha.metius.complex

import scala.{math => m}

final case class Complex(real: Double, imag: Double) {

  def abs: Double = m.hypot(real, imag)

  def arg: Double = m.atan2(imag, real)

  def squareNorm: Double = real * real + imag * imag

  def minus: Complex = Complex(-real, -imag)

  def conjugate: Complex = Complex(real, -imag)

  def unary_+ : Complex = this

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

  def /(other: Complex): Complex =
    Complex.divide(this.real, this.imag, other.real, other.imag)

  def +(x: Double): Complex = Complex(this.real + x, this.imag)

  def -(x: Double): Complex = Complex(this.real - x, this.imag)

  def *(x: Double): Complex = Complex(this.real * x, this.imag * x)

  def /(x: Double): Complex = Complex(this.real / x, this.imag / x)

  def square: Complex = Complex(real * real - imag * imag, 2.0 * real * imag)

  def sqrt: Complex = {
    if (real == 0.0) {
      if (imag == 0.0) Complex.zero
      else { // this branch insures for imaginary in the result real == +/- imag
        val t = m.sqrt(0.5 * m.abs(imag))
        if (imag > 0.0) Complex(t, t)
        else Complex(t, -t)
      }
    }
    else {
      val t = m.sqrt(0.5 * (m.abs(real) + abs))
      if (real >= 0) Complex(t, 0.5 * imag / t)
      else if (imag >= 0) Complex(0.5 * imag / t, t)
      else Complex(-0.5 * imag / t, -t)
    }
  }

  def exp: Complex = Complex.polar(m.exp(this.real), this.imag)

  def log: Complex = Complex(m.log(this.abs), this.arg)

  override def toString: String = {
    if (imag == 0.0) real.toString
    else if (real == 0.0) s"${imag.toString}i"
    else s"${real.toString}${if (imag > 0) "+" else ""}${imag.toString}i"
  }

}

object Complex {

  val zero = Complex(0.0, 0.0)

  val one = Complex(1.0, 0.0)

  val i = Complex(0.0, 1.0)

  def apply(real: Double): Complex = Complex(real, 0.0)

  def polar(mod: Double, arg: Double): Complex = Complex(mod * m.cos(arg), mod * m.sin(arg))

  def abs(z: Complex): Double = z.abs

  def square(z: Complex): Complex = z.square

  def sqrt(z: Complex): Complex = z.sqrt

  def exp(z: Complex): Complex = z.exp

  def log(z: Complex): Complex = z.log

  private def inverse(c: Double, d: Double): Complex = {
    if (c.isInfinite || d.isInfinite) Complex.zero
    else if (d == 0.0) Complex(1.0 / c, 0.0)
    else if (c == 0.0) Complex(0.0, -1.0 / d)
    else if (m.abs(c) >= m.abs(d)) {
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
    if (m.abs(c) >= m.abs(d)) divideOrdered(a, b, c, d)
    else divideOrdered(-b, a, -d, c)
  }

  private def divideOrdered(a: Double, b: Double, c: Double, d: Double): Complex = {
    if (c.isInfinite && !(a.isInfinite || b.isInfinite)) Complex.zero
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