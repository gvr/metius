package metius.random

trait PseudoRandomGenerator[@specialized(Int, Long) T <: Numeric[T]] {

  def get: T

  def next: PseudoRandomGenerator[T]

}

object PseudoRandomGenerator {



}