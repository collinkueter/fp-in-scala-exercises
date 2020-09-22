package com.collinkueter.exercises.laziness

import com.collinkueter.exercises.laziness
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class StreamTest extends AnyFlatSpec with Matchers {
  private val streamOf6 = laziness.Stream(1, 2, 3, 4, 5, 6)

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
    val f = (a: Int) => a < 4

    Empty.takeWhile(f).toList shouldBe List.empty
    laziness.Stream(1, 2, 3, 4, 5, 6).takeWhile(f).toList shouldBe List(1, 2, 3)
  }

  "exist" should "return true if element exists in stream" in {
    streamOf6.exists(i => i == 6) shouldBe true
    streamOf6.exists(i => i == 7) shouldBe false
  }

  "foldRight" should "return something" in {
    streamOf6.foldRight(0)((a, b) => a + b) shouldBe 21
  }

  "forAll" should "return true when all elements in the stream meet the condition" in {
    (Empty: laziness.Stream[Int]).forAll(i => i < 4) shouldBe true
    streamOf6.forAll(i => i < 4) shouldBe false
    streamOf6.forAll(i => i < 6) shouldBe false
    streamOf6.forAll(i => i < 7) shouldBe true
  }

  "takeWhileInTermsOfFoldRight" should "return elements matching predicate" in {
    val f = (a: Int) => a < 4

    Empty.takeWhileUnfold(f).toList shouldBe List.empty
    laziness.Stream(1, 2, 3, 4, 5, 6).takeWhileUnfold(f).toList shouldBe List(1, 2, 3)
  }

  "map" should "map over each element" in {
    streamOf6.map(i => i * 2).toList shouldBe List(2, 4, 6, 8, 10, 12)
  }

  "append" should "place element in the stream" in {
    val s = laziness.Stream(7, 8)
    streamOf6.append(s).toList shouldBe List(1, 2, 3, 4, 5, 6, 7, 8)
  }

  "prepend" should "place element on the beginning of the stream" in {
    val s = laziness.Stream(7, 8)
    streamOf6.prepend(s).toList shouldBe List(7, 8, 1, 2, 3, 4, 5, 6)
  }

  "flatMap" should "wrap each element in a new type" in {
    streamOf6.flatMap(i => laziness.Stream(i, i)).toList shouldBe List(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6)
  }

  "ones" should "generate an infinite stream of 1's" in {
    laziness.Stream.ones.take(5).toList shouldBe List(1, 1, 1, 1, 1)
  }

  // Exercise 5.8
  "constant" should "generate an infinite stream of a constant value" in {
    laziness.Stream.constant(4).take(3).toList shouldBe List(4, 4, 4)
  }

  "from" should "generate an infinite stream of a constant value + 1" in {
    laziness.Stream.from(4).take(3).toList shouldBe List(4, 5, 6)
  }

  "fromInTermsOfFoldRight" should "generate an infinite stream of a constant value + 1" in {
    laziness.Stream.fromUnfold(4).take(3).toList shouldBe List(4, 5, 6)
  }

  // Exercise 5.10
  "fibs" should "return fibonacci sequence" in {
    laziness.Stream.fibs.take(2).toList shouldBe List(0, 1)
    laziness.Stream.fibs.take(7).toList shouldBe List(0, 1, 1, 2, 3, 5, 8)
    laziness.Stream.fibs.take(22).toList shouldBe List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610, 987, 1597, 2584, 4181, 6765, 10946)
  }

  "fibsInTermsOfUnfold" should "return fibonacci sequence" in {
    laziness.Stream.fibUnfold.take(2).toList shouldBe List(0, 1)
    laziness.Stream.fibUnfold.take(7).toList shouldBe List(0, 1, 1, 2, 3, 5, 8)
    laziness.Stream.fibUnfold.take(22).toList shouldBe List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610, 987, 1597, 2584, 4181, 6765, 10946)
  }

  "fromInTermsOfUnfold" should "generate an infinite stream of a constant value + 1" in {
    laziness.Stream.fromUnfold(4).take(3).toList shouldBe List(4, 5, 6)
  }

  "constantInTermsOfUnfold" should "generate an infinite stream of a constant value" in {
    laziness.Stream.constantUnfold(4).take(3).toList shouldBe List(4, 4, 4)
  }

  "onesInTermsOfUnfold" should "generate an infinite stream of ones" in {
    laziness.Stream.onesUnfold.take(5).toList shouldBe List(1, 1, 1, 1, 1)
  }

  // Exercise 5.13
  behavior of "Exercise 5.13"

  "mapViaUnfold" should "map over each element" in {
    streamOf6.mapUnfold(i => i * 2).toList shouldBe List(2, 4, 6, 8, 10, 12)
  }

  "takeViaUnfold" should "return n number of elements from stream" in {
    Empty.takeUnfold(0).toList shouldBe List.empty
    streamOf6.takeUnfold(0).toList shouldBe List.empty
    streamOf6.takeUnfold(3).toList shouldBe List(1, 2, 3)
    streamOf6.takeUnfold(6).toList shouldBe List(1, 2, 3, 4, 5, 6)
    streamOf6.takeUnfold(7).toList shouldBe List(1, 2, 3, 4, 5, 6)
  }

  "takeWhileViaUnfold" should "return elements matching predicate" in {
    val f = (a: Int) => a < 4

    Empty.takeWhileUnfold(f).toList shouldBe List.empty
    laziness.Stream(1, 2, 3, 4, 5, 6).takeWhileUnfold(f).toList shouldBe List(1, 2, 3)
  }

  "zipWith" should "perform action with elements in each stream" in {
    val s1 = laziness.Stream("1", "2", "3")
    val s2 = laziness.Stream("3", "4", "5")
    s1.zipWith(s2)((a, b) => s"$a$b").toList shouldBe List("13", "24", "35")
  }

  "zipAll" should "perform action with elements in each stream" in {
    val s1 = laziness.Stream("1", "2", "3")
    val s2 = laziness.Stream("3", "4", "5", "6")
    s1.zipAll(s2).toList shouldBe List((Some("1"), Some("3")), (Some("2"), Some("4")), (Some("3"), Some("5")), (None, Some("6")))
  }

  "startsWith" should "check if one stream is a prefix of another" in {
    val s1 = laziness.Stream("1", "2", "3")
    val s2 = laziness.Stream("1", "2", "3", "4", "5", "6")
    s2.startsWith(s1) shouldBe true
    s1.startsWith(s2) shouldBe false
  }

  it should "work for infinite streams" in {
    laziness.Stream.ones.startsWith(laziness.Stream(1)) shouldBe true
  }
}
