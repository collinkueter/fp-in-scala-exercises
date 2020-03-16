package com.collinkueter.exercises.stream

import org.scalatest.diagrams.Diagrams
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class StreamTest extends AnyFlatSpec with Matchers with Diagrams {
  private val streamOf6 = Stream(1, 2, 3, 4, 5, 6)

  "headOption" should "return head if exists otherwise return None" in {
    streamOf6.headOption shouldBe Some(1)
    Empty.headOption shouldBe None
  }

  "toList" should "convert Stream to List" in {
    streamOf6.toList shouldBe List(1, 2, 3, 4, 5, 6)
  }

  "take" should "return n number of elements from stream" in {
    Empty.take(0).toList shouldBe List.empty
    streamOf6.take(0).toList shouldBe List.empty
    streamOf6.take(3).toList shouldBe List(1, 2, 3)
    streamOf6.take(6).toList shouldBe List(1, 2, 3, 4, 5, 6)
    streamOf6.take(7).toList shouldBe List(1, 2, 3, 4, 5, 6)
  }

  "drop" should "skip n number of elements and return the remaining from stream" in {
    Empty.drop(0).toList shouldBe List.empty
    streamOf6.drop(0).toList shouldBe List(1, 2, 3, 4, 5, 6)
    streamOf6.drop(3).toList shouldBe List(4, 5, 6)
    streamOf6.drop(6).toList shouldBe List.empty
    streamOf6.drop(7).toList shouldBe List.empty
  }

  "takeWhile" should "return elements matching predicate" in {
    val f = (a: Int) => a % 2 == 0

    Empty.takeWhile(f).toList shouldBe List.empty
    streamOf6.takeWhile(f).toList shouldBe List(2, 4, 6)
  }

  "exist" should "return true if element exists in stream" in {
    streamOf6.exists(i => i == 6) shouldBe true
    streamOf6.exists(i => i == 7) shouldBe false
  }

  "foldRight" should "return something" in {
    streamOf6.foldRight(0)((a, b) => a + b) shouldBe 21
  }

  "existInTermsOfFoldRight" should "return true if element exists in stream" in {
    streamOf6.existsInTermsOfFoldRight(i => i == 6) shouldBe true
    streamOf6.existsInTermsOfFoldRight(i => i == 7) shouldBe false
  }

  "forAll" should "return true when stream is empty" in {
    (Empty: Stream[Int]).forAll(i => i < 4) shouldBe true
  }

  it should "return false when does not satisfy < 4" in {
    streamOf6.forAll(i => i < 4) shouldBe false
  }

  it should "return false when does not satisfy < 6" in {
    streamOf6.forAll(i => i < 6) shouldBe false
  }

  it should "return false when does not satisfy < 7" in {
    streamOf6.forAll(i => i < 7) shouldBe true
  }

  "takeWhileInTermsOfFoldRight" should "return elements matching predicate" in {
    val f = (a: Int) => a % 2 == 0
    Empty.takeWhileInTermsOfFoldRight(f).toList shouldBe List.empty
    streamOf6.takeWhileInTermsOfFoldRight(f).toList shouldBe List(2, 4, 6)
  }

  "headOptionInTermsOfFoldRight" should "return head if exists otherwise return None" in {
    streamOf6.headOptionInTermsOfFoldRight shouldBe Some(1)
    Empty.headOptionInTermsOfFoldRight shouldBe None
  }
}
