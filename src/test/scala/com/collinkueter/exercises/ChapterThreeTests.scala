package com.collinkueter.exercises

import org.scalatest._

class ChapterThreeTests
    extends FlatSpec
    with Matchers
    with DiagrammedAssertions {

  behavior of "3.1 pattern matching"

  it should "be equal to 3" in {
    ChapterThree.x shouldBe 3
  }

  behavior of "tail"

  it should "remove the first element from the list" in {
    List.tail(List(1, 2, 3, 4)) shouldBe List(2, 3, 4)
  }

  behavior of "setHead"

  it should "replace the first element in the list" in {
    List.setHead(List(1, 2, 3, 4), 2) shouldBe List(2, 2, 3, 4)
  }

  behavior of "drop"

  it should "return Nil when list is Nil" in {
    List.drop(Nil, 1) shouldBe Nil
  }

  it should "remove the 1st element from the list" in {
    val l = List(1, 2, 3, 4)
    List.drop(l, 1) shouldBe List(2, 3, 4)
  }

  it should "remove the zero elements from the list" in {
    val l = List(1, 2, 3, 4)
    List.drop(l, 0) shouldBe List(1, 2, 3, 4)
  }

  it should "remove the 3 elements from the list" in {
    val l = List(1, 2, 3, 4)
    List.drop(l, 3) shouldBe List(4)
  }

  behavior of "dropWhile"

  it should "return Nil when list is Nil" in {
    List.dropWhile(Nil, (a: Int) => a == 1) shouldBe Nil
  }

  it should "remove while less than 3" in {
    val l = List(1, 2, 3, 4, 5)
    val f = (a: Int) => a < 3
    List.dropWhile(l, f) shouldBe List(3, 4, 5)
  }

  behavior of "init"

  it should "return Nil when list is Nil" in {
    List.init(Nil) shouldBe Nil
  }

  it should "remove the last element from the list" in {
    List.init(List(1, 2, 3, 4, 5)) shouldBe List(1, 2, 3, 4)
    List.init(List(1)) shouldBe Nil
  }

  behavior of "3.8"

  it should "be the same list when you pass Nil and Cons to foldRight" in {
    List.foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _)) shouldBe List(
      1,
      2,
      3
    )
  }

  behavior of "length"

  it should "be a length of 5" in {
    List.length(Nil) shouldBe 0
    List.length(List(1)) shouldBe 1
    List.length(List(1, 2, 3)) shouldBe 3
    List.length(List(1, 2, 3, 4, 5)) shouldBe 5
  }

  behavior of "3.10"

  it should "reverse the list when you pass Nil and Cons to foldLeft" in {
    List.foldLeft(List(1, 2, 3), Nil: List[Int])((b, a) => Cons(a, b)) shouldBe List(
      3,
      2,
      1
    )
  }

  behavior of "sum in terms of foldLeft"

  it should "add elements in the list" in {
    List.sumInTermsOfFoldLeft(Nil: List[Int]) shouldBe 0
    List.sumInTermsOfFoldLeft(List(1)) shouldBe 1
    List.sumInTermsOfFoldLeft(List(1, 2, 3, 4, 5)) shouldBe 15
  }

  behavior of "product in terms of foldLeft"

  it should "multiply elements in the list" in {
    List.productInTermsOfFoldLeft(Nil: List[Int]) shouldBe 0
    List.productInTermsOfFoldLeft(List(1)) shouldBe 1
    List.productInTermsOfFoldLeft(List(1, 2, 3, 4, 5)) shouldBe 120
  }

  behavior of "length in terms of foldLeft"

  it should "be a length of 5" in {
    List.lengthInTermsOfFoldLeft(Nil: List[Int]) shouldBe 0
    List.lengthInTermsOfFoldLeft(List(1)) shouldBe 1
    List.lengthInTermsOfFoldLeft(List(1, 2, 3)) shouldBe 3
    List.lengthInTermsOfFoldLeft(List(1, 2, 3, 4, 5)) shouldBe 5
  }

  behavior of "reverse"

  it should "reverse the list" in {
    List.reverse(List(1, 2, 3)) shouldBe List(3, 2, 1)
  }

  behavior of "foldRight in terms of foldLeft"

  ignore should "construct the same list when passed nil and cons" in {
    val l = List(1, 2, 3)
    List.foldRightInTermsOfFoldLeft(l, Nil: List[Int])((a, b) => Cons(a, b)) shouldBe l
  }

  behavior of "foldLeft in terms of foldRight"

  it should "construct the same list reversed when passed nil and cons" in {
    val l = List(1, 2, 3)
    val rl = List(3, 2, 1)
    List.foldLeftInTermsOfFoldRight(l, Nil: List[Int])((b, a) => Cons(a, b)) shouldBe rl
  }

  behavior of "appendElement"

  it should "add element to the end of the list" in {
    List.appendElement(List(1, 2, 3), 4) shouldBe List(1, 2, 3, 4)
  }

  behavior of "append"

  it should "add one list to the end of the list" in {
    List.append(List(1, 2, 3), List(4, 5, 6)) shouldBe List(1, 2, 3, 4, 5, 6)
  }

  behavior of "concat"

  ignore should "take list of list and turn into a list of elements" in {
    val l = List(List(1, 2), List(3, 4), List(5, 6))
    List.concat(l) shouldBe List(1, 2, 3, 4, 5, 6)
  }

  behavior of "addOne"

  it should "add one to each element in the list" in {
    List.addOne(List(1, 2, 3)) shouldBe List(2, 3, 4)
  }

  behavior of "doubleToString"

  it should "turn each double into a string" in {
    val l = List(1.0, 2.0, 3.0)
    List.doubleToString(l) shouldBe List("1.0", "2.0", "3.0")
  }

  behavior of "map"

  it should "add one to each element of the list" in {
    List.map(List(1, 2, 3))(a => a + 1) shouldBe List(2, 3, 4)
  }

  behavior of "filter"

  it should "remove odd numbers from the list" in {
    val l = List(1, 2, 3, 4, 5, 6, 7, 8)
    val expected = List(2, 4, 6, 8)
    List.filter(l)(a => a % 2 == 0) shouldBe expected
  }

  behavior of "flatMap"

  it should "create a list with two elements" in {
    val expected = List(1, 1, 2, 2, 3, 3)
    List.flatMap(List(1, 2, 3))(i => List(i, i)) shouldBe expected
  }

  behavior of "filter in terms of flatmap"

  it should "remove odd numbers from the list" in {
    val l = List(1, 2, 3, 4, 5, 6, 7, 8)
    val expected = List(2, 4, 6, 8)
    List.filterInTermsOfFlatMap(l)(a => a % 2 == 0) shouldBe expected
  }

  behavior of "addLists"

  it should "add elements in list together" in {
    val l1 = List(1, 2, 3)
    val l2 = List(4, 5, 6)
    List.addLists(l1, l2) shouldBe List(5, 7, 9)
  }

  behavior of "zipWith"

  it should "perform action with elements in two list" in {
    val l1 = List("1", "2", "3")
    val l2 = List("3", "4", "5")
    List.zipWith(l1, l2)((a, b) => s"$a$b") shouldBe List("13", "24", "35")
  }

  behavior of "subSequence"

  it should "find subsequence within a list = List(1, 2)" in {
    List.hasSubsequence(List(1, 2, 3, 4), List(1, 2)) shouldBe true
  }
  
  it should "find subsequence within a list in List(2, 3)" in {
    List.hasSubsequence(List(1, 2, 3, 4), List(2, 3)) shouldBe true
  }

  it should "find subsequence within a list in List(4)" in {
    List.hasSubsequence(List(1, 2, 3, 4), List(4)) shouldBe true
  }
}
