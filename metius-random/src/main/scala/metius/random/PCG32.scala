package metius.random

import java.lang.Integer.rotateRight

final class PCG32 private(state: Long, increment: Long) extends PseudoRandomGenerator[Int] {

  import PCG32._

  override def get: Int = scramble(state)

  override def next: PCG32 = {
    val state = advanceState(state, increment)
    new PCG32(state, increment)
  }

}

object PCG32 {

  // see http://www.pcg-random.org
  // code https://github.com/imneme/pcg-c-basic/blob/master/pcg_basic.c

  private val multiplier: Long = 6364136223846793005L

  private val increment: Long = 1442695040888963407L

  @inline private def advanceState(state: Long, increment: Long): Long =
    multiplier * state + increment

  @inline private def scramble(state: Long): Int = {
    val xss: Int = (((state >>> 18) ^ state) >>> 27).toInt
    val rot: Int = (state >>> 59).toInt
    rotateRight(xss, rot)
  }

  def init(seed: Long, sequence: Long = increment): PCG32 = {
    val increment = (sequence << 1L) | 1L
    val state = advanceState(increment + seed, increment)
    new PCG32(state, increment)
  }

  final class Mutable(initState: Long, increment: Long) {

    private var state = initState

    def next(): Int = {
      state = advanceState(state, increment)
      scramble(state)
    }

  }

  def randomInts(state: Long, increment: Long): Mutable = {
    new Mutable(state, increment)
  }

}
