package com.collinkueter.exercises
import java.awt.datatransfer.FlavorMap

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil              => 0
    case Cons(head, tail) => head + sum(tail)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil              => 1.0
    case Cons(head, tail) => head * product(tail)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  // Exercise 3.2
  def tail[A](as: List[A]): List[A] = as match {
    case Nil              => Nil
    case Cons(head, tail) => tail
  }

  // Exercise 3.3
  def setHead[A](as: List[A], a: A): List[A] = as match {
    case Nil              => Nil
    case Cons(head, tail) => Cons(a, tail)
  }

  // Exercise 3.4
  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Nil => Nil
    case Cons(head, tail) =>
      n match {
        case 0 => l
        case x => drop(tail, x - 1)
      }
  }

  // Exercise 3.5
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil                           => Nil
    case Cons(head, tail) if (f(head)) => dropWhile(tail, f)
    case xs                            => xs
  }

  // Exercise 3.6
  def init[A](l: List[A]): List[A] = l match {
    case Nil                               => Nil
    case Cons(head, tail) if (tail == Nil) => Nil
    case Cons(head, tail)                  => Cons(head, init(tail))
  }

  // Listing 3.2
  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil              => z
      case Cons(head, tail) => f(head, foldRight(tail, z)(f))
    }

  def sum2(ns: List[Int]) = foldRight(ns, 0)(_ + _)
  def product2(ns: List[Double]) = foldRight(ns, 1.0)(_ * _)

  // Exercise 3.7
  def productWithHalt(ns: List[Double]) = ???

  // Exercise 3.9
  def length[A](as: List[A]): Int = foldRight(as, 0)((_, b) => b + 1)

  // Exercise 3.10
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil              => z
    case Cons(head, tail) => foldLeft(tail, f(z, head))(f)
  }

  // Exercise 3.11
  def sumInTermsOfFoldLeft(l: List[Int]): Int = foldLeft(l, 0)(_ + _)

  // Exercise 3.11
  def productInTermsOfFoldLeft(l: List[Int]): Int = l match {
    case Nil => 0
    case _   => foldLeft(l, 1)(_ * _)
  }

  // Exercise 3.11
  def lengthInTermsOfFoldLeft(l: List[Int]): Int =
    foldLeft(l, 0)((b, _) => b + 1)

  // Exercise 3.12
  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil: List[A])((b, a) => Cons(a, b))

  // Exercise 3.13 - HARD
  def foldRightInTermsOfFoldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    ???

  // Exercise 3.13 - HARD
  def foldLeftInTermsOfFoldRight[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    foldRight(reverse(as), z)((a, b) => f(b, a))

  // Exercise 3.14
  def append[A](l1: List[A], l2: List[A]): List[A] =
    foldRight(l1, l2)(Cons(_, _))

  // Exercise 3.14 - Bonus not in book
  def appendElement[A](l: List[A], a: A): List[A] =
    foldRight(l, Cons(a, Nil))(Cons(_, _))

  // Exercise 3.15 - HARD
  def concat[A](l: List[List[A]]): List[A] =
    foldRight(l, Nil: List[A])((a, b) => append(a, b))

  // Exercise 3.16
  def addOne(l: List[Int]): List[Int] =
    foldRight(l, Nil: List[Int])((a, b) => Cons(a + 1, b))

  // Exercise 3.17
  def doubleToString(l: List[Double]): List[String] =
    foldRight(l, Nil: List[String])((a, b) => Cons(a.toString(), b))

  // Exercise 3.18
  def map[A, B](as: List[A])(f: A => B): List[B] =
    foldRight(as, Nil: List[B])((a, b) => Cons(f(a), b))

  // Exercise 3.19
  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, Nil: List[A])((a, b) => if (f(a)) Cons(a, b) else b)

  // Exercise 3.20
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = concat(map(as)(f))

  // Exercise 3.21
  def filterInTermsOfFlatMap[A](as: List[A])(f: A => Boolean): List[A] = {
    flatMap(as)((a: A) => if (f(a)) Cons(a, Nil) else Nil)
  }

  // Exercise 3.22
  def addLists(l1: List[Int], l2: List[Int]): List[Int] = (l1, l2) match {
    case (Nil, _)                     => Nil
    case (_, Nil)                     => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addLists(t1, t2))
  }

  // Exercise 3.23
  def zipWith[A](l1: List[A], l2: List[A])(f: (A, A) => A): List[A] =
    (l1, l2) match {
      case (Nil, _)                     => Nil
      case (_, Nil)                     => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
    }

  // Exercise 3.24
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    def startsWith[A](supI: List[A], suI: List[A]): Boolean =
      (supI, suI) match {
        case (_, Nil)                                => true
        case (Cons(h, t), Cons(h2, t2)) if (h == h2) => startsWith(t, t2)
        case _                                       => false
      }
    sup match {
      case Nil                         => sub == Nil
      case _ if (startsWith(sup, sub)) => true
      case Cons(h, t)                  => hasSubsequence(t, sub)
    }
  }
}

object ChapterThree {
  // Exercise 3.1
  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _)))          => x
    case Nil                                   => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t)                            => h + List.sum(t)
    case _                                     => 101
  }
}

sealed trait CrappyTree[+A]
case class Leaf[A](value: A) extends CrappyTree[A]
case class Branch[A](left: CrappyTree[A], right: CrappyTree[A]) extends CrappyTree[A]

object CrappyTreeExercises {
  // Exercise 3.25
  def size[A](tree: CrappyTree[A]): Int =
    tree match {
      case Leaf(d)             => 1
      case Branch(left, right) => size(left) + size(right)
    }

  // Exercise 3.26
  def maximum(tree: CrappyTree[Int]): Int = tree match {
    case Leaf(value)         => value
    case Branch(left, right) => maximum(left) max maximum(right)
  }

  // Exercise 3.27
  def depth[A](tree: CrappyTree[A]): Int = tree match {
    case Leaf(value)         => 1
    case Branch(left, right) => (depth(left) max depth(right)) + 1
  }

  // Exercise 3.28
  def map[A, B](tree: CrappyTree[A])(f: A => B): CrappyTree[B] = tree match {
    case Leaf(value)         => Leaf(f(value))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }

  // Exercise 3.29-a
  def fold[A, B](tree: CrappyTree[A])(f: A => B)(g: (B, B) => B): B =
    tree match {
      case Leaf(a)             => f(a)
      case Branch(left, right) => g(fold(left)(f)(g), fold(right)(f)(g))
    }

  // Exercise 3.29-b 
  def sizeInTermsOfFold[A](tree: CrappyTree[A]): Int = fold(tree)(_ => 1)((b1, b2) => b1 + b2)

  // Exercise 3.29-c
  def maximumInTermsOfFold(tree: CrappyTree[Int]): Int = fold(tree)(a => a)((b1, b2) => b1 max b2)

  // Exercise 3.29-d
  def depthInTermsOfFold[A](tree: CrappyTree[A]): Int = fold(tree)(_ => 1)((b1, b2) => (b1 max b2) + 1)
}
