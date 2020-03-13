package com.collinkueter.exercises

import scala.collection.immutable.{List => SList}

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = flatMap(f.andThen(Right(_)))

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Left(value)  => Left(value)
    case Right(value) => f(value)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(_)  => b
    case Right(_) => this
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    for {
      a <- this
      bb <- b
    } yield f(a, bb)

//  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
//
//  }
}

case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+E](value: E) extends Either[Nothing, E]

object Either {
  def sequence[E, A](es: SList[Either[E, A]]): Either[E, SList[A]] = traverse(es)(identity)

  def traverse[E, A, B](as: SList[A])(f: A => Either[E, B]): Either[E, SList[B]] =
    as.foldRight(Right(SList.empty[B]): Either[E, SList[B]]) { (h, t) =>
//      t.map2(f(h))((bs, b) => b :: bs)
      f(h).map2(t)(_ :: _)
    }
}
