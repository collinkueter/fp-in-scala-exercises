package com.collinkueter.exercises.state

import com.collinkueter.exercises.state.Machine.{simulateMachine, tagCandies, tagCoins}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MachineTest extends AnyFlatSpec with Matchers {
  val inputCoin: List[Input] = List(Coin)
  val inputTurn: List[Input] = List(Turn)

  val machine1: Machine = Machine(locked = true, candies = tagCandies(1), coins = tagCoins(0))
  val machine2: Machine = Machine(locked = false, candies = tagCandies(1), coins = tagCoins(1))
  val machine3: Machine = Machine(locked = true, candies = tagCandies(0), coins = tagCoins(1))

  "Inserting a coin into a locked machine" should "cause it to unlock if there’s any candy left." in {
    val result = simulateMachine(inputCoin).run(machine1)
    result._2.locked shouldBe false
    result._2.candies shouldBe 1
    result._2.coins shouldBe 1
  }

  "Turning the knob on an unlocked machine" should "cause it to dispense candy and become locked" in {
    val result = simulateMachine(inputTurn).run(machine2)
    result._2.locked shouldBe true
    result._2.candies shouldBe 0
    result._2.coins shouldBe 1
  }

  "Turning the knob on a locked machine or inserting a coin into an unlocked machine" should "do nothing" in {
    val result1 = simulateMachine(inputTurn).run(machine1)
    result1._2.locked shouldBe machine1.locked
    result1._2.candies shouldBe 1
    result1._2.coins shouldBe 0

    val result2 = simulateMachine(inputCoin).run(machine2)
    result2._2.locked shouldBe machine2.locked
    result2._2.candies shouldBe 1
    result2._2.coins shouldBe 1
  }

  "A machine that’s out of candy" should "ignore all inputs" in {
    val result = simulateMachine(inputTurn).run(machine3)
    result._2.locked shouldBe machine3.locked
    result._2.candies shouldBe 0
    result._2.coins shouldBe 1
  }

  "Book example" should "work" in {
    val machine = Machine(locked = true, candies = tagCandies(5), coins = tagCoins(10))
    val inputs = List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn)
    val result = simulateMachine(inputs).run(machine)
    result._2.locked shouldBe true
    result._2.coins shouldBe 14
    result._2.candies shouldBe 1
  }
}
