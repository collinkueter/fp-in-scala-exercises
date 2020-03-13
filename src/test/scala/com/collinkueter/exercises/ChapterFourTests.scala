package com.collinkueter.exercises

package com.collinkueter

import org.scalatest._

class ChapterFourTests extends FlatSpec with Matchers with DiagrammedAssertions {
  behavior of "Option"

  it should "map if exists" in {
    Some(2).map(_ * 2) shouldBe Some(4)
    None.map(f => f) shouldBe None
  }

  it should "flatmap if exists" in {
    Some(2).flatMap(a => Some(a * 2)) shouldBe Some(4)
    None.flatMap(f => f) shouldBe None
  }

  it should "getOrElse" in {
    Some(2).getOrElse(1) shouldBe 2
    None.getOrElse(1) shouldBe 1
  }

  it should "orElse" in {
    Some(2).orElse(Some(1)) shouldBe Some(2)
    None.orElse(Some(1)) shouldBe Some(1)
  }

  it should "filter" in {
    Some(2).filter(a => a == 2) shouldBe Some(2)
    Some(2).filter(a => a == 1) shouldBe None
    None.filter(a => a == 2) shouldBe None
  }

  it should "isDefined" in {
    Some(2).isDefined shouldBe true
    None.isDefined shouldBe false
  }

//  behavior of "variance"
//
//  it should "return" in {
//    val doubles = Seq(1.0, 2.0, 3.0)
//    ChapterFour.variance(doubles) shouldBe None
//  }
}
