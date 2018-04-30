package com.github.gvr.metius.real

object Polynomial {

  // replace with Math.fma for Java 9+
  private def fma(a: Double, b: Double, c: Double): Double = a * b + c

  @inline def horner(as: Seq[Double]): Double => Double = x =>
    as.foldRight(0.0) { (a: Double, acc: Double) => fma(x, acc, a) }

  @inline def apply(a0: Double): Double => Double = _ => a0

  @inline def apply(a0: Double, a1: Double): Double => Double = x => a0 + x * a1

  @inline def apply(a0: Double, a1: Double, a2: Double): Double => Double = x => a0 + x * (a1 + x * a2)

  @inline def apply(a0: Double, a1: Double, a2: Double, a3: Double): Double => Double = x =>
    a0 + x * (a1 + x * (a2 + x * a3))

  @inline def apply(a0: Double, a1: Double, a2: Double, a3: Double, a4: Double): Double => Double = x =>
    a0 + x * (a1 + x * (a2 + x * (a3 + x * a4)))

  @inline def apply(a0: Double, a1: Double, a2: Double, a3: Double, a4: Double, a5: Double): Double => Double = x =>
    a0 + x * (a1 + x * (a2 + x * (a3 + x * (a4 + x * a5))))

  @inline def apply(a0: Double, a1: Double, a2: Double, a3: Double, a4: Double, a5: Double, a6: Double): Double => Double = x =>
    a0 + x * (a1 + x * (a2 + x * (a3 + x * (a4 + x * (a5 + x * a6)))))

  @inline def apply(a0: Double, a1: Double, a2: Double, a3: Double, a4: Double, a5: Double, a6: Double, a7: Double): Double => Double = x =>
    a0 + x * (a1 + x * (a2 + x * (a3 + x * (a4 + x * (a5 + x * (a6 + x * a7))))))

  @inline def apply(a0: Double, a1: Double, a2: Double, a3: Double, a4: Double, a5: Double, a6: Double, a7: Double, a8: Double): Double => Double = x =>
    a0 + x * (a1 + x * (a2 + x * (a3 + x * (a4 + x * (a5 + x * (a6 + x * (a7 + x * a8)))))))

  @inline def apply(a0: Double, a1: Double, a2: Double, a3: Double, a4: Double, a5: Double, a6: Double, a7: Double, a8: Double, a9: Double): Double => Double = x =>
    a0 + x * (a1 + x * (a2 + x * (a3 + x * (a4 + x * (a5 + x * (a6 + x * (a7 + x * (a8 + x * a9))))))))

  @inline def apply(a0: Double, a1: Double, a2: Double, a3: Double, a4: Double, a5: Double, a6: Double, a7: Double, a8: Double, a9: Double, a10: Double): Double => Double = x =>
    a0 + x * (a1 + x * (a2 + x * (a3 + x * (a4 + x * (a5 + x * (a6 + x * (a7 + x * (a8 + x * (a9 + x * a10)))))))))

  @inline  def apply(a0: Double, a1: Double, a2: Double, a3: Double, a4: Double, a5: Double, a6: Double, a7: Double, a8: Double, a9: Double, a10: Double, a11: Double): Double => Double = x =>
    a0 + x * (a1 + x * (a2 + x * (a3 + x * (a4 + x * (a5 + x * (a6 + x * (a7 + x * (a8 + x * (a9 + x * (a10 + x * a11))))))))))

  @inline def apply(a0: Double, a1: Double, a2: Double, a3: Double, a4: Double, a5: Double, a6: Double, a7: Double, a8: Double, a9: Double, a10: Double, a11: Double, a12: Double): Double => Double = x =>
    a0 + x * (a1 + x * (a2 + x * (a3 + x * (a4 + x * (a5 + x * (a6 + x * (a7 + x * (a8 + x * (a9 + x * (a10 + x * (a11 + x * a12)))))))))))

  @inline def apply(a0: Double, a1: Double, a2: Double, a3: Double, a4: Double, a5: Double, a6: Double, a7: Double, a8: Double, a9: Double, a10: Double, a11: Double, a12: Double, a13: Double): Double => Double = x =>
    a0 + x * (a1 + x * (a2 + x * (a3 + x * (a4 + x * (a5 + x * (a6 + x * (a7 + x * (a8 + x * (a9 + x * (a10 + x * (a11 + x * (a12 + x * a13))))))))))))

  @inline def apply(a0: Double, a1: Double, a2: Double, a3: Double, a4: Double, a5: Double, a6: Double, a7: Double, a8: Double, a9: Double, a10: Double, a11: Double, a12: Double, a13: Double, a14: Double): Double => Double = x =>
    a0 + x * (a1 + x * (a2 + x * (a3 + x * (a4 + x * (a5 + x * (a6 + x * (a7 + x * (a8 + x * (a9 + x * (a10 + x * (a11 + x * (a12 + x * (a13 + x * a14)))))))))))))

  @inline def apply(a0: Double, a1: Double, a2: Double, a3: Double, a4: Double, a5: Double, a6: Double, a7: Double, a8: Double, a9: Double, a10: Double, a11: Double, a12: Double, a13: Double, a14: Double, a15: Double): Double => Double = x =>
    a0 + x * (a1 + x * (a2 + x * (a3 + x * (a4 + x * (a5 + x * (a6 + x * (a7 + x * (a8 + x * (a9 + x * (a10 + x * (a11 + x * (a12 + x * (a13 + x * (a14 + x * a15))))))))))))))

  @inline def apply(a0: Double, a1: Double, a2: Double, a3: Double, a4: Double, a5: Double, a6: Double, a7: Double, a8: Double, a9: Double, a10: Double, a11: Double, a12: Double, a13: Double, a14: Double, a15: Double, a16: Double, as: Double*): Double => Double = {
    val p = horner(as)
    (x: Double) => a0 + x * (a1 + x * (a2 + x * (a3 + x * (a4 + x * (a5 + x * (a6 + x * (a7 + x * (a8 + x * (a9 + x * (a10 + x * (a11 + x * (a12 + x * (a13 + x * (a14 + x * (a15 + x * (a16 + x * p(x)))))))))))))))))
  }

}
