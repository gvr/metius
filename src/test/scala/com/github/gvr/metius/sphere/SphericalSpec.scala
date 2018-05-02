package com.github.gvr.metius.sphere

import org.scalatest.{Matchers, WordSpec}

class SphericalSpec extends WordSpec with Matchers {

  "Spherical" should {

    import Spherical._

    "provide the opposite of an angle in 0 ≤ α ≤ 2π" in {
      oppositeAngle(0.0) shouldBe math.Pi
      oppositeAngle(1.0) shouldBe 1.0 + math.Pi
      oppositeAngle(4.0) shouldBe 4.0 - math.Pi
      oppositeAngle(math.Pi) shouldBe 0.0
      oppositeAngle(2 * math.Pi) shouldBe math.Pi
    }

  }

}
