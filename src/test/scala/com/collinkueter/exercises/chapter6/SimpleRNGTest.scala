package com.collinkueter.exercises.chapter6

import com.collinkueter.exercises.chapter6.RNG.Simple
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SimpleRNGTest extends AnyFlatSpec with Matchers {
  "nextInt" should "return same value for same seed" in {
    val rng = RNG.Simple(42)
    val (n1, rng2) = rng.nextInt

    rng should not be rng2
    n1 shouldBe rng.nextInt._1
    rng2.nextInt._1 shouldBe rng2.nextInt._1
  }

  "nonNegativeInt" should "return a non negative random integer between 0 and Int.MaxValue" in {
    val rng = Simple(47)
    val (result1, rng1) = RNG.nonNegativeInt(rng)
    val result2 = RNG.nonNegativeInt(rng1)._1

    result1 should be >= 0
    result2 should be >= 0
    result1 should not be result2
  }

  "double" should "return double random number" in {
    val rng = Simple(47)
    val (double1, rng1) = RNG.double(rng)
    val double2 = RNG.double(rng1)._1

    double1.toInt should be >= 0
    double2.toInt should be >= 0
    double1 should not be double2
  }

  "ints" should "return list of random integers" in {
    val (list1, rng1) = RNG.ints(5)(Simple(47))
    val list2 = RNG.ints(5)(rng1)._1
    list1.size shouldBe 5
    list1.headOption should not be list2
  }

  "ints2" should "return list of random integers" in {
    val (list1, rng1) = RNG.ints2(5)(Simple(47))
    val list2 = RNG.ints2(5)(rng1)._1
    list1.size shouldBe 5
    list1.headOption should not be list2
  }

  "doubleM" should "return random double" in {
    val rng = Simple(47)
    val (double1, rng2) = RNG.double(rng)
    val double2 = RNG.double(rng2)._1

    double1.toInt should be >= 0
    double2.toInt should be >= 0
    double1 should not be double2
  }

  "map2" should "take two rands and return single rand" in {
    val rng = Simple(47)
    val ra = RNG.double
    val rb = RNG.double

    RNG.map2(ra, rb)((a: Double, b: Double) => a + b)(rng)._1 shouldBe (0.008420645259320736 + 0.712942339014262)
  }

  "nonNegativeLessThan" should "produce a non-negative number less than a value" in {
    val (result1, rng1) = RNG.nonNegativeLessThan(10)(Simple(47))
    val result2 = RNG.nonNegativeLessThan(10)(rng1)._1

    result1 should be >= 0
    result1 should be < 10
    result2 should be >= 0
    result2 should be < 10
    result1 should not be result2
  }
}
