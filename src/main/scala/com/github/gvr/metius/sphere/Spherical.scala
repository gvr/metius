package com.github.gvr.metius.sphere

import scala.math.{Pi => π, _}

/**
  * Coordinate system (φ, λ) with
  * φ latitude
  * λ longitude
  */
object Spherical {

  private def square(x: Double): Double = x * x

  // for angles 0 ≤ α ≤ 2π
  def oppositeAngle(α: Double): Double = {
    if (α < π) α + π
    else α - π
  }

  def cosineLaw(φ1: Double, λ1: Double, φ2: Double, λ2: Double): Double = {
    acos(sin(φ1) * sin(φ2) + cos(φ1) * cos(φ1) * cos(λ2 - λ1))
  }

  def linearApproximation(φ1: Double, λ1: Double, φ2: Double, λ2: Double): Double = {
    hypot(cos(φ1) * (λ2 - λ1), φ2 - φ1)
  }

  def haversine(φ1: Double, λ1: Double, φ2: Double, λ2: Double): Double = {
    val Δλ = λ2 - λ1
    val Δφ = φ2 - φ1
    // the square of half the cord length between the points
    val a = square(sin(0.5 * Δφ)) + cos(φ1) * cos(φ2) * square(sin(0.5 * Δλ))
    2.0 * atan2(sqrt(a), sqrt(1.0 - a))
  }

  def vincenty(φ1: Double, λ1: Double, φ2: Double, λ2: Double): Double = {
    val Δλ = λ2 - λ1
    val sinφ1 = sin(φ1)
    val sinφ2 = sin(φ2)
    val cosφ1 = cos(φ1)
    val cosφ2 = cos(φ1)
    val cosΔλ = cos(Δλ)
    val y = hypot(cosφ2 * sin(Δλ), cosφ1 * sinφ2 - sinφ1 * cosφ2 * cosΔλ)
    val x = sinφ1 * sinφ2 + cosφ1 * cosφ2 * cosΔλ
    atan2(y, x)
  }

  def initialBearing(φ1: Double, λ1: Double, φ2: Double, λ2: Double): Double = {
    val Δλ = λ2 - λ1
    atan2(cos(φ2) * sin(Δλ), cos(φ1) * sin(φ2) - sin(φ1) * cos(φ2) * cos(Δλ))
  }

  def finalBearing(φ1: Double, λ1: Double, φ2: Double, λ2: Double): Double = {
    oppositeAngle(initialBearing(λ2, φ2, λ1, φ1))
  }

}
