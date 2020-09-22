package com.collinkueter.exercises.datastructures

import scala.annotation.tailrec

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def nil[A]: List[A] = Nil

  def sum(ints: List[Int]): Int = ints match {
    case Nil         => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil         => 1.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](list: List[A]): List[A] = list match {
    case Cons(_, tail) => tail
    case Nil           => Nil
  }

  def setHead[A](list: List[A], head: A): List[A] = list match {
    case Cons(_, tail) => Cons(head, tail)
    case Nil           => Cons(head, Nil)
  }

  @tailrec
  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Cons(_, tail) if n == 1 => tail
    case Cons(_, tail) if n > 1  => drop(tail, n - 1)
    case Nil                     => Nil
  }

  @tailrec
  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Cons(h, t) if f(h) => dropWhile(t)(f)
    case _                  => l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Cons(_, Nil) | Nil => Nil
    case Cons(h, t)         => Cons(h, init(t))
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil         => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]): Int = foldRight(ns, 0)(_ + _)

  def product2(ns: List[Double]): Double = foldRight(ns, 1.0)(_ * _)

  def shortcircuitingFoldRight[A, B](as: List[A], z: B)(shortCircuit: A => Boolean)(f: (A, B) => B): B = as match {
    case Nil                           => z
    case Cons(x, _) if shortCircuit(x) => f(x, z)
    case Cons(x, xs) =>
      f(x, shortcircuitingFoldRight(xs, z)(shortCircuit)(f))
  }

  def len[A](as: List[A]): Int =
    foldRight(as, 0)((_, b) => b + 1)

  @tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    as match {
      case Nil        => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }

  def sumLeft(as: List[Int]): Int = foldLeft(as, 0)(_ + _)

  def productLeft(as: List[Double]): Double = foldLeft(as, 1.0)(_ * _)

  def lenLeft[A](as: List[A]): Int = foldLeft(as, 0)((i, _) => i + 1)

  def reverse[A](as: List[A]): List[A] = foldLeft(as, nil[A])((b, a) => Cons(a, b))

  def foldRightViaFoldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(as), z)((b, a) => f(a, b))

  def foldLeftViaFoldRight[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    foldRight(as, identity[B](_))((a, g) => b => g(f(b, a)))(z)

  def append[A](as: List[A], bs: List[A]): List[A] =
    foldLeft(reverse(as), bs) { (l, e) =>
      Cons(e, l)
    }

  def appendElement[A](l: List[A], a: A): List[A] =
    foldRight(l, Cons(a, Nil))(Cons(_, _))

  def concat[A](l: List[List[A]]): List[A] =
    foldRight(l, Nil: List[A])((a, b) => append(a, b))

  def flatten[A](lists: List[List[A]]): List[A] =
    //    foldLeft(lists, nil[A])(append)
    flatMap(lists)(identity)

  def mapPlus1(ints: List[Int]): List[Int] =
    foldRight(ints, nil[Int])((a, b) => Cons(a + 1, b))

  def doublesToStrings(ds: List[Double]): List[String] =
    foldRight(ds, nil[String])((a, b) => Cons(a.toString, b))

  def map[A, B](as: List[A])(f: A => B): List[B] =
    foldRight(as, nil[B])((a, b) => Cons(f(a), b))

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, nil[A]) { (a, b) =>
      if (f(a)) Cons(a, b) else b
    }

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    foldRight(as, nil[B]) { (a, b) =>
      append(f(a), b)
    }

  def filterWithFlatMap[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(a => if (f(a)) List(a) else nil)

  def addLists(l1: List[Int], l2: List[Int]): List[Int] = (l1, l2) match {
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addLists(t1, t2))
    case _                            => Nil
  }

  def zipAdd(l1: List[Int], l2: List[Int]): List[Int] = zipWith(l1, l2)(_ + _)

  def zipWith[A, B](l1: List[A], l2: List[A])(f: (A, A) => B): List[B] = (l1, l2) match {
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
    case _                            => Nil
  }

  @scala.annotation.tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    @scala.annotation.tailrec
    def startsWith(supInner: List[A], subInner: List[A]): Boolean = (supInner, subInner) match {
      case (_, Nil)                                 => true
      case (Cons(h1, t1), Cons(h2, t2)) if h1 == h2 => startsWith(t1, t2)
      case _                                        => false
    }

    sup match {
      case Nil        => false
      case Cons(_, t) => startsWith(sup, sub) || hasSubsequence(t, sub)
    }
  }

  //  @scala.annotation.tailrec
  //  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
  //    @scala.annotation.tailrec
  //    def inner(supInner: List[A], subInner: List[A]): Boolean = (supInner, subInner) match {
  //      case (_, Nil) => true
  //      case (Cons(h1, t1), Cons(h2, t2)) if h1 == h2 => inner(t1, t2)
  //      case _ => false
  //    }
  //
  //    sup match {
  //      case Nil => (sub == Nil)
  //      case _ if inner(sup, sub) => true
  //      case Cons(_, t) => hasSubsequence(t, sub)
  //    }
  //  }

  //  @scala.annotation.tailrec
  //  def startsWith[A](sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
  //    case (Cons(h1, t1), Cons(h2, t2)) if h1 == h2 => startsWith(t1, t2)
  //    case (_, Nil) => true
  //    case _ => false
  //  }
  //
  //  @scala.annotation.tailrec
  //  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
  //    case (Cons(h1, t1), Cons(h2, t2)) if h1 == h2 => startsWith(t1, t2)
  //    case (Cons(_, t1), _) => hasSubsequence(t1, sub)
  //    case (_, Nil) => true
  //    case _ => false
  //  }
}
