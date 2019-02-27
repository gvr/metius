package metius.random

import java.lang.Long.bitCount

final class SplitMix private(seed: Long, gamma: Long) {

  import SplitMix._

  def getLong: Long = mix13(seed)

  def getInt: Int = mix04(seed).toInt

  def next: SplitMix = new SplitMix(seed + gamma, gamma)

  def split: SplitMix = {
    val newGamma = mixGamma(seed + gamma)
    new SplitMix(mix13(seed) + newGamma, newGamma)
  }

}

// reference: Steele, Lea & Flood (2014) Fast Splittable Pseudorandom Number Generators

object SplitMix {

  private val goldenGamma: Long = 0x9e3779b97f4a7c15L

  // reference: David Stafford, http://zimbry.blogspot.com/2011/09/better-bit-mixing-improving-on.html
  @inline private def mix04(n: Long): Long = {
    var m = n
    m ^= m >>> 33
    m *= 0x62a9d9ed799705f5L
    m ^= m >>> 28
    m *= 0xcb24d0a5c88c35b3L
    m ^= m >>> 32
    m
  }

  // reference: David Stafford, http://zimbry.blogspot.com/2011/09/better-bit-mixing-improving-on.html
  @inline private def mix13(n: Long): Long = {
    var m = n
    m ^= m >>> 30
    m *= 0xbf58476d1ce4e5b9L
    m ^= m >>> 27
    m *= 0x94d049bb133111ebL
    m ^= m >>> 31
    m
  }

  // reference: Austin Appleby, https://github.com/aappleby/smhasher/wiki/MurmurHash3
  @inline private def murmurHash3(n: Long): Long = {
    var m = n
    m ^= m >>> 33
    m *= 0xff51afd7ed558ccdL
    m ^= m >>> 33
    m *= 0xc4ceb9fe1a85ec53L
    m ^= m >>> 33
    m
  }

  @inline private def mixGamma(n: Long): Long = {
    val m = murmurHash3(n) | 1L
    val bits = bitCount(m ^ (m >>> 1))
    if (bits < 24) m ^ 0xaaaaaaaaaaaaaaaaL
    else m
  }

  def init(seed: Long, gamma: Long = goldenGamma): SplitMix = {
    new SplitMix(seed + gamma, gamma)
  }

}
