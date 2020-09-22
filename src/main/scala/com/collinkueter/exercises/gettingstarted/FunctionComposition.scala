package com.collinkueter.exercises.gettingstarted

object FunctionComposition {
  def compose[A, B, C](f: B => C, g: A => B): A => C = a => f(g(a))
}
