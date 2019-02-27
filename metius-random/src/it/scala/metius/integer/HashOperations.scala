package metius.integer

object HashOperations {

  implicit class IntOperations(val underlying: Int) extends AnyVal {

    def murmurHash3: Int = {
      var z = underlying
      z = (z ^ (z >>> 16)) * 0x85ebca6b
      z = (z ^ (z >>> 13)) * 0xc2b2ae35
      z ^ (z >>> 16)
    }

  }

  implicit class LongOperations(val underlying: Long) extends AnyVal {

    def murmurHash3: Long = Mixer.murmurHash3(underlying) //{
//      var z = underlying
//      z ^= z >>> 33
//      z *= 0xff51afd7ed558ccdL
//      z ^= z >>> 33
//      z *= 0xc4ceb9fe1a85ec53L
//      z ^= z >>> 33
//      z
//    }

  }

}
