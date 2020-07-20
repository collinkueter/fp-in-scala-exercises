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
    case Cons(h, t) if p(h()) => Cons(h, () => t().takeWhile(p))
    case _                    => Empty
  }

  def filter(p: A => Boolean): Stream[A] = this match {
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
  def forAll(p: A => Boolean): Boolean =
    this match {
      case Empty                => true
      case Cons(h, t) if p(h()) => t().forAll(p)
      case _                    => false
    }

  def forAllInTermsOfFoldRight(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  // Exercise 5.5
  def takeWhileInTermsOfFoldRight(p: A => Boolean): Stream[A] =
    this.foldRight(Empty: Stream[A])((a: A, b) => if (p(a)) Stream.cons(a, b) else Stream.empty)

  // Exercise 5.6
  def headOptionInTermsOfFoldRight: Option[A] = this.foldRight(None: Option[A])((a, _) => Some(a))

  // Exercise 5.7
  def map[B](f: A => B): Stream[B] = foldRight(Empty: Stream[B])((a: A, b) => Stream.cons(f(a), b))

  def filterInTermsOfFoldRight(p: A => Boolean): Stream[A] =
    this.foldRight(Empty: Stream[A])((a: A, b) => if (p(a)) Stream.cons(a, b) else b)

  def append[B >: A](s: => Stream[B]): Stream[B] = foldRight(s)((a: A, b) => Stream.cons(a, b))

  def prepend[B >: A](s: => Stream[B]): Stream[B] =
    s.foldRight(this.asInstanceOf[Stream[B]])((a, b) => Stream.cons(a, b))

  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(Stream.empty[B])((a: A, b) => f(a).append(b))

  def find(p: A => Boolean): Option[A] = filter(p).headOption

  // Exercise 5.13
  def mapViaUnfold[B](f: A => B): Stream[B] =
    Stream.unfold(this) {
      case Cons(h, t) =>
        val output: B = f(h())
        val nextState: Stream[A] = t()
        Some((output, nextState))
      case Empty => None
    }

  def takeViaUnfold(n: Int): Stream[A] = Stream.unfold((n, this)) {
    case (0, _)          => None
    case (i, Cons(h, t)) => Some((h(), (i - 1, t())))
    case (_, Empty)      => None
  }

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] = Stream.unfold(this) {
    case Cons(h, t) if p(h()) => Some((h(), t()))
    case _                    => None
  }

  def zipWith[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] = Stream.unfold((this, s2)) {
    case (Empty, _) | (_, Empty) => None
    case (Cons(h1, t1), Cons(h2, t2)) =>
      val output = f(h1(), h2())
      val nextState = (t1(), t2())
      Some((output, nextState))
  }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = Stream.unfold((this, s2)) {
    case (Empty, Empty) => None
    case (s1, s2) =>
      val output = (s1.headOption, s2.headOption)
      val nextState = (s1.drop(1), s2.drop(1))
      Some((output, nextState))
//    case (Empty, Cons(h2, t2)) =>
//      val output = (None, Some(h2()))
//      val nextState = (Stream.empty, t2())
//      Some((output, nextState))
//    case (Cons(h1, t1), Empty) =>
//      val output = (Some(h1()), None)
//      val nextState = (t1(), Stream.empty)
//      Some((output, nextState))
//    case (Cons(h1, t1), Cons(h2, t2)) =>
//      val output = (Some(h1()), Some(h2()))
//      val nextState = (t1(), t2())
//      Some((output, nextState))
  }

  def startsWith[AA >: A](s: Stream[AA]): Boolean = {
//    val (s1, s2) = Stream.unzip(zipAll(s))
//    s1.zipWith(s2.takeWhile(_.isDefined))(
//        (oa, ob) =>
//          (for {
//            a <- oa
//            b <- ob
//          } yield a == b).getOrElse(false)
//      )
//      .foldRight(true)(_ && _)
    zipAll(s).foldRight(true) {
      case ((Some(a), Some(b)), acc) => a == b && acc
      case ((_, None), acc)          => acc
      case ((None, _), _)            => false
    }
  }
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def unzip[A, B](s: Stream[(A, B)]): (Stream[A], Stream[B]) = s match {
    case Empty => (empty, empty)
    case Cons(h, t) =>
      val (a, b) = h()
      val (t1, t2) = unzip(t())
      (cons(a, t1), cons(b, t2))
  }

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  def ones: Stream[Int] = Cons(() => 1, () => ones)

  def oneTwo: Stream[Int] = Cons(() => 1, () => Cons(() => 2, () => Empty: Stream[Int]): Stream[Int])

  // Exercise 5.8
  def constant[B](a: B): Stream[B] = {
    lazy val s: Stream[B] = Cons(() => a, () => s)
    s
  }

  // Exercise 5.9
  def from(n: Int): Stream[Int] = Cons(() => n, () => from(n + 1))

  // TODO: Come back to after 5.13
  def fromInTermsOfFoldRight(n: Int): Stream[Int] = Cons(() => n, () => from(n + 1))

  // Exercise 5.10
  def fibs(): Stream[Int] = {
    def go(n1: Int, n2: Int): Stream[Int] = Cons(() => n1, () => go(n2, n2 + n1))
    go(0, 1)
  }

  // Exercise 5.11
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case None               => Stream.empty
    case Some((a: A, s: S)) => Cons(() => a, () => unfold(s)(f))
  }

  // Exercise 5.12
  def fibsInTermsOfUnfold(): Stream[Int] = unfold((0, 1)) {
    case (output, n2) =>
      val nextA = n2
      val nextB = output + n2
      Some((output, (nextA, nextB)))
//    Some((n1, (n2, n1 + n2)))
  }

  def fromInTermsOfUnfold(n: Int): Stream[Int] = unfold(n)(s => Some((s, s + 1)))

  def constantInTermsOfUnfold(n: Int): Stream[Int] = unfold(n)(s => Some(s, s))

  def onesInTermsOfUnfold(): Stream[Int] = unfold(1)(s => Some(s, s))
}
