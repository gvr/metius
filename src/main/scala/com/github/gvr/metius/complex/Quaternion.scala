package com.github.gvr.metius.complex

import scala.{math => m}

final case class Quaternion(real: Double, i: Double, j: Double, k: Double) {

  def imag: Double = m.hypot(m.hypot(i, j), k)

  def abs: Double = m.hypot(real, imag)

  def arg: Double = m.atan2(imag, real)

  def squareNorm: Double = real * real + i * i + j * j + k * k

  def minus: Quaternion = Quaternion(-real, -i, -j, -k)

  def conjugate: Quaternion = Quaternion(real, -i, -j, -k)

  def inverse: Quaternion = {
    val x = 1.0 / squareNorm
    Quaternion(real * x, -i * x, -j * x, -k * x)
  }

  def unary_+ : Quaternion = this

  def unary_- : Quaternion = minus

  def unary_~ : Quaternion = conjugate

  def +(other: Quaternion): Quaternion =
    Quaternion(this.real + other.real, this.i + other.i, this.j + other.j, this.k + other.k)

  def -(other: Quaternion): Quaternion =
    Quaternion(this.real - other.real, this.i - other.i, this.j - other.j, this.k - other.k)

  def *(other: Quaternion): Quaternion =
    Quaternion(
      this.real * other.real - this.i * other.i - this.j * other.j - this.k * other.k,
      this.real * other.i + this.i * other.real + this.j * other.k - this.k * other.j,
      this.real * other.j - this.i * other.k + this.j * other.real + this.k * other.i,
      this.real * other.k + this.i * other.j - this.j * other.i + this.k * other.real
    )

  def /(other: Quaternion): Quaternion = {
    val x = 1.0 / other.abs
    val qa = Quaternion(this.real * x, this.i * x, this.j * x, this.k * x)
    val qb = Quaternion(other.real * x, -other.i * x, -other.j * x, -other.k * x)
    qa * qb
  }

  def +(x: Double): Quaternion = Quaternion(real + x, i, j, k)

  def -(x: Double): Quaternion = Quaternion(real - x, i, j, k)

  def *(x: Double): Quaternion = Quaternion(real * x, i * x, j * x, k * x)

  def /(x: Double): Quaternion = Quaternion(real / x, i / x, j / x, k / x)

  def square: Quaternion = {
    val r2 = real + real
    Quaternion(real * real - i * i - j * j - k * k, r2 * i, r2 * j, r2 * k)
  }

  def sqrt: Quaternion = {
    val im = imag
    if (im == 0) {
      if (real == 0.0) Quaternion.zero
      else if (real > 0.0) Quaternion(m.sqrt(real))
      else Quaternion(0.0, m.sqrt(-real), 0.0, 0.0) // actually all sqrt(real) * u with u arbitray unit size compnination of i,j,k are roots
    }
    else if (real == 0.0) {
      val t = m.sqrt(0.5 * im)
      val s = t / im
      Quaternion(t, i * s, j * s, k * s)
    }
    else {
      val t = m.sqrt(0.5 * (m.abs(real) + abs))
      if (real >= 0) {
        val s = 0.5 / t
        Quaternion(t, i * s, j * s, k * s)
      }
      else {
        val s = t / im
        Quaternion(0.5 / s, i * s, j * s, k * s)
      }
    }
  }

  def exp: Quaternion = {
    val r = m.exp(real)
    val θ = imag
    if (θ / r == 0.0) Quaternion(r)
    else {
      val s = m.sin(θ) * (r / θ)
      Quaternion(r * m.cos(θ), i * s, j * s, k * s)
    }
  }

  def log: Quaternion = {
    val s = imag
    if (s == 0.0) Quaternion(m.log(m.abs(real)))
    else {
      val t = m.atan2(s, real) / s
      Quaternion(m.log(abs), i * t, j * t, k * t)
    }
  }

  private def signedString(x: Double, postfix: String): String =
    if (x == 0.0) ""
    else if (x < 0.0) s"${x.toString}$postfix"
    else s"+${x.toString}$postfix"

  override def toString: String = {
    val s = (if (real != 0.0) real.toString else "") +
      signedString(i, "i") +
      signedString(j, "j") +
      signedString(k, "k")
    if (s.isEmpty) real.toString
    else if (s.startsWith("+")) s.substring(1)
    else s
  }

}

object Quaternion {

  val zero: Quaternion = Quaternion(0.0, 0.0, 0.0, 0.0)

  val one: Quaternion = Quaternion(1.0, 0.0, 0.0, 0.0)

  val i: Quaternion = Quaternion(0.0, 1.0, 0.0, 0.0)

  val j: Quaternion = Quaternion(0.0, 0.0, 1.0, 0.0)

  val k: Quaternion = Quaternion(0.0, 0.0, 0.0, 1.0)

  def apply(r: Double): Quaternion = Quaternion(r, 0.0, 0.0, 0.0)

  def abs(q: Quaternion): Double = q.abs

  def square(q: Quaternion): Quaternion = q.square

  def sqrt(q: Quaternion): Quaternion = q.sqrt

  def exp(q: Quaternion): Quaternion = q.exp

  def log(q: Quaternion): Quaternion = q.log

}
