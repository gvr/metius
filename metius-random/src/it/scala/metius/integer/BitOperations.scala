package metius.integer

object BitOperations {

  implicit class IntOperations(val underlying: Int) extends AnyVal {

    def rotateLeft(n: Int): Int = java.lang.Integer.rotateLeft(underlying, n)

    def rotateRight(n: Int): Int = java.lang.Integer.rotateRight(underlying, n)

  }

  implicit class LongOperations(val underlying: Long) extends AnyVal {

    def rotateLeft(n: Int): Long = java.lang.Long.rotateLeft(underlying, n)

    def rotateRight(n: Int): Long = java.lang.Long.rotateRight(underlying, n)

  }


}
