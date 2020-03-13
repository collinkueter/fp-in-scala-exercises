package com.collinkueter.exercises

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(get) => Some(f(get))
    case None      => None
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case Some(get) => f(get)
    case None      => None
  }

  def getOrElse[B >: A](default: => B): B = if (this == None) default else this.get

  def orElse[B >: A](ob: => Option[B]): Option[B] = if (this == None) ob else this

  def filter(f: A => Boolean): Option[A] = {
    if (this == None) None
    else if (f(this.get)) this
    else None
  }

  // Exercise 4.3
  def map2[B, C, D](a: Option[B], b: Option[C])(f: (B, C) => D): Option[D] =
    for {
      av <- a
      bv <- b
    } yield f(av, bv)

  // Exercise 4.4
  def sequence[B](a: scala.collection.immutable.List[Option[B]]): Option[scala.collection.immutable.List[B]] =
    a match {
      case Some(head) :: tail             => sequence(tail).map(sequencedTail => head :: sequencedTail)
      case scala.collection.immutable.Nil => Some(scala.collection.immutable.List.empty)
      case _                              => None
    }

  def isDefined: Boolean = this != None

  def get: A
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing] {
  def get = throw new UnsupportedOperationException("get not defined on None")
}

object ChapterFour {
  def mean(xs: Seq[Double]): Option[Double] = if (xs.isEmpty) None else Some(xs.sum / xs.length)

  // Exercise 4.2
  def variance(xs: Seq[Double]): Option[Double] = mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

  def variance2(xs: Seq[Double]): Option[Double] =
    for {
      m <- mean(xs)
      v <- mean(xs.map(x => math.pow(x - m, 2)))
    } yield v
}
