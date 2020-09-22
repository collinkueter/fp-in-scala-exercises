package com.collinkueter.exercises.gettingstarted

import com.collinkueter.exercises.gettingstarted.Currying.{curry, uncurry}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CurryingTest extends AnyFlatSpec with Matchers {
  behavior of "curry"

  it should "curry" in {
    def f(a: Int, b: Int): Int = a + b
    def g(a: Int)(b: Int): Int = a + b

    curry(f)(1)(1) == f(1, 1) shouldBe true
    curry(f)(1)(1) == g(1)(1) shouldBe true
    curry(f)(1)(1) shouldBe 2
    f(1, 1) shouldBe 2
  }

  behavior of "uncurry"

  it should "uncurry" in {
    def f(a: Int, b: Int): Int = a + b
    def g(a: Int)(b: Int): Int = a + b

    uncurry(g)(1, 1) == g(1)(1) shouldBe true
    uncurry(g)(1, 1) == f(1, 1) shouldBe true
  }
}
