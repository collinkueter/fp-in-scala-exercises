package com.collinkueter.exercises

object ChapterTwo {

  // Exercise 2.1
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

  // Exercise 2.2
  def isSorted[A](as: Array[A])(ordered: (A, A) => Boolean): Boolean = {
    def go(i: Int): Boolean = i match {
      case 0                                    => true
      case 1                                    => true
      case _ if (ordered(as(i - 2), as(i - 1))) => go(i - 1)
      case _                                    => false
    }

    go(as.length)
  }

  // Exercise 2.3
  def curry[A, B, C](f: (A, B) => C): A => (B => C) =
    a => b => f(a, b)

  // Exercise 2.4
  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a: A, b: B) => f(a)(b)

  // Exercise 2.5
  def compose[A, B, C](f: B => C, g: A => B): A => C = a => f(g(a))
}
