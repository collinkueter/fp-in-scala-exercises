package com.collinkueter.exercises

package com.collinkueter

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

  def isDefined: Boolean = this != None

  def get: A
}

case class Some[A](get: A) extends Option[A]
case object None extends Option[Nothing] {
  def get = throw new UnsupportedOperationException("get not defined on None")
}
