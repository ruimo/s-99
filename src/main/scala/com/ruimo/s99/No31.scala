package com.ruimo.s99

object No31 {
  def isPrime(n: BigInt): Boolean = 
    if (n < 0) throw new Error("n(=%d) should >=0".format(n))
    else if (n == BigInt(0)) false
    else if (n == BigInt(1)) false
    else if (n == BigInt(2)) true
    else if (n == BigInt(3)) true
    else if (n % 2 == 0) false
    else _isPrime(n, 3)

  private def _isPrime(n: BigInt, d: BigInt): Boolean =
    if (d * d > n) true
    else if (n % d == 0) false
    else _isPrime(n, d + 2)
}
