package metius.integer

object Mixer {



  // references:

  // reference: Austin Appleby, https://github.com/aappleby/smhasher/wiki/MurmurHash3
  def murmurHash3(n: Int): Int = {
    var m = n
    m ^= m >>> 16
    m *= 0x85ebca6b
    m ^= m >>> 13
    m *= 0xc2b2ae35
    m ^= m >>> 16
    m
  }

  // reference: Austin Appleby, https://github.com/aappleby/smhasher/wiki/MurmurHash3
  def murmurHash3(n: Long): Long = {
    var m = n
    m ^= m >>> 33
    m *= 0xff51afd7ed558ccdL
    m ^= m >>> 33
    m *= 0xc4ceb9fe1a85ec53L
    m ^= m >>> 33
    m
  }

  // reference: David Stafford, http://zimbry.blogspot.com/2011/09/better-bit-mixing-improving-on.html
  def mix04(n: Long): Long = {
    var m = n
    m ^= m >>> 33
    m *= 0x62a9d9ed799705f5L
    m ^= m >>> 28
    m *= 0xcb24d0a5c88c35b3L
    m ^= m >>> 32
    m
  }

  // reference: David Stafford, http://zimbry.blogspot.com/2011/09/better-bit-mixing-improving-on.html
  def mix13(n: Long): Long = {
    var m = n
    m ^= m >>> 30
    m *= 0xbf58476d1ce4e5b9L
    m ^= m >>> 27
    m *= 0x94d049bb133111ebL
    m ^= m >>> 31
    m
  }

  @inline private final def ror(n: Long, distance: Int): Long = java.lang.Long.rotateRight(n, distance)

  // reference: Pelle Evensen,
  // https://mostlymangling.blogspot.com/2018/07/on-mixing-functions-in-fast-splittable.html
  def rrmxmx(n: Long): Long = {
    var v = n
    v ^= ror(v,49) ^ ror(v, 24)
    v *= 0x9fb21c651e98df25L
    v ^= v >>> 28
    v *= 0x9fb21c651e98df25L
    v ^ v >>> 28
  }

  // reference: Pelle Evensen,
  // https://mostlymangling.blogspot.com/2019/01/better-stronger-mixer-and-test-procedure.html
  def rrxmrrxmsx(n: Long): Long = {
    var v = n
    v ^= ror(v,25) ^ ror(v, 50)
    v *= 0xa24baed4963ee407L
    v ^= ror(v,24) ^ ror(v, 49)
    v *= 0x9fb21c651e98df25L
    v ^ v >>> 28
  }

}
