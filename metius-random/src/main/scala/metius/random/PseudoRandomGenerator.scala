package metius.random

trait PseudoRandomGenerator[@specialized(Int, Long) T] {

  def get: T

  def next: PseudoRandomGenerator[T]

}

object PseudoRandomGenerator {



}