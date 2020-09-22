package com.collinkueter.exercises.errorhandling

import com.collinkueter.exercises.errorhandling.{None => CNone, Some => CSome}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class OptionTest extends AnyFlatSpec with Matchers {
  behavior of "Option"

  it should "map if exists" in {
    CSome(2).map(_ * 2) shouldBe CSome(4)
    CNone.map(f => f) shouldBe CNone
  }

  it should "flatmap if exists" in {
    CSome(2).flatMap(a => CSome(a * 2)) shouldBe CSome(4)
    CNone.flatMap(f => f) shouldBe CNone
  }

  it should "getOrElse" in {
    CSome(2).getOrElse(1) shouldBe 2
    CNone.getOrElse(1) shouldBe 1
  }

  it should "orElse" in {
    CSome(2).orElse(CSome(1)) shouldBe CSome(2)
    CNone.orElse(CSome(1)) shouldBe CSome(1)
  }

  it should "filter" in {
    CSome(2).filter(a => a == 2) shouldBe CSome(2)
    CSome(2).filter(a => a == 1) shouldBe CNone
    CNone.filter(a => a == 2) shouldBe CNone
  }

  it should "isDefined" in {
    CSome(2).isDefined shouldBe true
    CNone.isDefined shouldBe false
  }
}
