package com.collinkueter.excercises

object ChapterTwo {

  def fib(n: Int): Int = {
    def go(n: Int, lastTwo: (Int, Int)): Int = {
      (n, lastTwo) match {
        case (0, (a, _)) => a
        case (1, (_, b)) => b
        case (_, (a, b)) => go(n - 1, (b, a + b))
      }
    }
    go(n, (0, 1))
  }

  def isSorted[A](as: Array[A])(ordered: (A, A) => Boolean): Boolean = {
    def go(i: Int): Boolean = i match {
      case 0                                    => true
      case 1                                    => true
      case _ if (ordered(as(i - 2), as(i - 1))) => go(i - 1)
      case _                                    => false
    }

    go(as.length)
  }
}
