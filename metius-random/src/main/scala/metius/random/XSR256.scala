package metius.random

import java.lang.Long.rotateLeft

final class XSR256 private (state0: Long, state1: Long, state2: Long, state3: Long) {

  import XSR256._

  def get: Long = scramble(state1)

  def next: XSR256 = {
    val s2 = state2 ^ state0
    val s3 = state3 ^ state1
    new XSR256(
      state0 = state0 ^ s3,
      state1 = state1 ^ s2,
      state2 = state2 ^ (state1 << 17L),
      state3 = rotateLeft(s3, 45)
    )
  }

}

object XSR256 {

  @inline private def scramble(state: Long): Long = {
    var result = rotateLeft(state + (state << 2L), 7)
    result + (result << 3L)
  }

  private class State {

    var s0: Long = 0
    var s1: Long = 0
    var s2: Long = 0
    var s3: Long = 0

    def next(): Unit = {
      val oldState = s1
      s2 ^= s0
      s3 ^= s1
      s1 ^= s2
      s0 ^= s3
      s2 ^= oldState << 17L
      s3 = rotateLeft(s3, 45)
    }

  }


}