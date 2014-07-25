package com.ruimo.s99

import No31.isPrime

class No31Test {
  @Test def test {
    assertFalse(isPrime(1))
    assertTrue(isPrime(2))
    assertTrue(isPrime(3))
    assertFalse(isPrime(4))
    assertTrue(isPrime(5))
    assertFalse(isPrime(6))
    assertTrue(isPrime(5))
    assertFalse(isPrime(8))
    assertFalse(isPrime(9))
    assertFalse(isPrime(10))
    assertTrue(isPrime(11))
    assertFalse(isPrime(12))
    assertTrue(isPrime(13))
  }
}
