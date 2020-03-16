package com.collinkueter.exercises.stream

sealed trait Stream[+A] {
  def headOption: Option[A] = {
    this match {
      case Empty      => None
      case Cons(h, _) => Some(h())
    }
  }

  // Exercise 5.1
  def toList: List[A] = this match {
    case Empty      => List.empty
    case Cons(h, t) => h() :: t().toList
  }

  // Exercise 5.2
  def take(n: Int): Stream[A] =
    if (n != 0) {
      this match {
        case Empty                => Empty
        case Cons(h, t) if n > 0  => Cons(h, () => t().take(n - 1))
        case Cons(h, _) if n == 0 => Cons(h, () => Stream.empty)
      }
    } else {
      Empty
    }

  // Exercise 5.2
  def drop(n: Int): Stream[A] = this match {
    case Empty               => Empty
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _                   => this
    //    case Cons(_, _) if n > 0  => s
    //    case Cons(_, t) if n > 0  => t().drop(n - 1)
    //    case Cons(h, t) if n <= 0 => Cons(h, () => t().drop(n - 1)) // co-recursive
  }

  // Exercise 5.3
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Empty                => Empty
    case Cons(h, t) if p(h()) => Cons(h, () => t().takeWhile(p))
    case Cons(_, t)           => t().takeWhile(p)
  }

  @scala.annotation.tailrec
  final def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _          => false
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _          => z
    }

  def existsInTermsOfFoldRight[B](p: A => Boolean): Boolean = foldRight(false)((a, b) => p(a) || b)

  // Exercise 5.4
  def forAll(p: A => Boolean): Boolean = this match {
    case Empty                => true
    case Cons(h, t) if p(h()) => t().forAll(p)
    case _                    => false
  }

  // Exercise 5.5
  def takeWhileInTermsOfFoldRight(p: A => Boolean): Stream[A] =
    this.foldRight(Empty: Stream[A])((a: A, b) => if (p(a)) Stream.cons(a, b) else b)

  // Exercise 5.6
  def headOptionInTermsOfFoldRight: Option[A] = this.foldRight(None: Option[A])((a, _) => Some(a))
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  def ones: Stream[Int] = Cons(() => 1, () => ones)

  def oneTwo: Stream[Int] = Cons(() => 1, () => Cons(() => 2, () => Empty: Stream[Int]): Stream[Int])
}
