package com.collinkueter.exercises.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  // Exercise 3.25
  def size[A](tree: Tree[A]): Int = fold(tree)(_ => 1)(_ + _ + 1)
//  tree match {
//    case Leaf(_)             => 1
//    case Branch(left, right) => size(left) + size(right) + 1
//  }

  // Exercise 3.26
  def maximum(tree: Tree[Int]): Int = fold(tree)(identity)(_ max _)
//  tree match {
//    case Leaf(value)         => value
//    case Branch(left, right) => maximum(left) max maximum(right)
//  }

  // Exercise 3.27
  def depth[A](tree: Tree[A]): Int = fold(tree)(_ => 1)(_ max _ + 1)
//  tree match {
//    case Leaf(_)         => 1
//    case Branch(left, right) => (depth(left) max depth(right)) + 1
//  }

  // Exercise 3.28
  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = fold(tree)(a => Leaf(f(a)): Tree[B])(Branch(_, _))
//  tree match {
//    case Leaf(value)         => Leaf(f(value))
//    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
//  }

  // Exercise 3.29-a
  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
    case Leaf(a)      => f(a)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }
}
