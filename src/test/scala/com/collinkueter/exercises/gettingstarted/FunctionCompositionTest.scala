package com.collinkueter.exercises.gettingstarted

import com.collinkueter.exercises.gettingstarted.FunctionComposition.compose
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FunctionCompositionTest extends AnyFlatSpec with Matchers {
  it should "compose" in {
    def f(b: Int): Int = b / 2
    def g(a: Int): Int = a + 2

    compose(f, g)(0) == compose(g, f)(0) shouldBe false
    compose(f, g)(2) shouldBe 2
    compose(g, f)(2) shouldBe 3
  }
}
