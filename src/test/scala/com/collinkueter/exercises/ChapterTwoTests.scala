package com.collinkueter.excercises

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

}
