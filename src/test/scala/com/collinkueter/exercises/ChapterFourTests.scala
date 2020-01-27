package com.collinkueter.exercises

package com.collinkueter

import org.scalatest._

class ChapterFourTests extends FlatSpec with Matchers with DiagrammedAssertions {
  behavior of "chapter 4"

  it should "be true" in {
    true shouldBe true
  }
}
