package cen.alpha.metius.complex

import scala.{math => m}

final case class Quaternion(real: Double, i: Double, j: Double, k: Double) {

  def abs: Double = m.hypot(m.hypot(real, i), m.hypot(j, k))

  def minus: Quaternion = Quaternion(-real, -i, -j, -k)

  def conjugate: Quaternion = Quaternion(real, -i, -j, -k)

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

  private def signedString(x: Double, postfix: String): String =
    if (x > 0.0) s"+${x.toString}$postfix"
    else if (x < 0.0) s"${x.toString}$postfix"
    else ""

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

  def abs(q: Quaternion): Double = q.abs

}