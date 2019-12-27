package com.collinkueter.exercises

import org.scalatest._

class ChapterTwoTests extends FlatSpec with Matchers with DiagrammedAssertions {

  behavior of "fib"

  it should "be 0 when fib(0)" in {
    ChapterTwo.fib(0) shouldBe 0
  }

  it should "be 1 when fib(1)" in {
    ChapterTwo.fib(1) shouldBe 1
  }

  it should "be 1 when fib(2)" in {
    ChapterTwo.fib(2) shouldBe 1
  }

  it should "be 5 when fib(5)" in {
    ChapterTwo.fib(3) shouldBe 2
  }

  it should "be 102334155 when fib(40)" in {
    ChapterTwo.fib(40) shouldBe 102334155
  }

  behavior of "isSorted"

  it should "be true when isSorted(List(1,2,3))" in {
    val input = Array(0, 1, 2, 3)
    ChapterTwo.isSorted(input)(_ < _) shouldBe true
  }

  it should "be false when isSorted(List(3,2,1))" in {
    val input = Array(0, 2, 1, 3)
    ChapterTwo.isSorted(input)(_ < _) shouldBe false
  }

  behavior of "curry"

  it should "curry" in {
    def f(a: Int, b: Int): Int = a + b
    def g(a: Int)(b: Int): Int = a + b

    ChapterTwo.curry(f)(1)(1) == f(1, 1) shouldBe true
    ChapterTwo.curry(f)(1)(1) == g(1)(1) shouldBe true
    ChapterTwo.curry(f)(1)(1) shouldBe 2
    f(1, 1) shouldBe 2
  }

  behavior of "uncurry"

  it should "uncurry" in {
    def f(a: Int, b: Int): Int = a + b
    def g(a: Int)(b: Int): Int = a + b

    ChapterTwo.uncurry(g)(1, 1) == g(1)(1) shouldBe true
    ChapterTwo.uncurry(g)(1, 1) == f(1, 1) shouldBe true
  }

  it should "compose" in {
    def f(b: Int): Int = b / 2
    def g(a: Int): Int = a + 2

    ChapterTwo.compose(f, g)(0) == ChapterTwo.compose(g, f)(0) shouldBe false
    ChapterTwo.compose(f, g)(2) shouldBe 2
    ChapterTwo.compose(g, f)(2) shouldBe 3
  }

}
