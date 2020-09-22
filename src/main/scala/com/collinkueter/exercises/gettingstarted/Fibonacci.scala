package com.collinkueter.exercises.gettingstarted

import scala.annotation.tailrec

object Fibonacci {
  def fibTailRec(n: Int): Int = {
    @tailrec
    def go(n: Int, lastTwo: (Int, Int)): Int = {
      (n, lastTwo) match {
        case (0, (a, _)) => a
        case (1, (_, b)) => b
        case (_, (a, b)) => go(n - 1, (b, a + b))
      }
    }
    go(n, (0, 1))
  }

  def fib(i: Int): Long = fibTailRec(i)
}
