package com.collinkueter.exercises.chapter6

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {
  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = Simple(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  // Exercise 6.1
//  def nonNegativeInt(rng: RNG): (Int, RNG) = {
//    val (i, nextRng) = rng.nextInt
//    (if (i < 0) -(i + 1) else i, nextRng)
//  }

  def nonNegativeInt: Rand[Int] = rng => {
    val (i, nextRng) = rng.nextInt
    (if (i < 0) -(i + 1) else i, nextRng)
  }

  // Exercise 6.2
//  def double(rng: RNG): (Double, RNG) = {
//    val (i, newRng) = nonNegativeInt(rng)
//    (i / (Integer.MAX_VALUE.toDouble + 1), newRng)
//  }

  // Exercise 6.3
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r) = rng.nextInt
    val (d, r2) = double(r)
    ((i, d), r2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (d, r) = double(rng)
    val (i, r2) = r.nextInt
    ((d, i), r2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)

    ((d1, d2, d3), r3)
  }

  // Exercise 6.4
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count == 0) (List.empty[Int], rng)
    else {
      val (head, rh) = rng.nextInt
      val (tail, rt) = ints(count - 1)(rh)
      (head :: tail, rt)
    }
  }

  // A tail-recursive solution
  def ints2(count: Int)(rng: RNG): (List[Int], RNG) = {
    def go(count: Int, r: RNG, xs: List[Int]): (List[Int], RNG) =
      if (count == 0)
        (xs, r)
      else {
        val (x, r2) = r.nextInt
        go(count - 1, r2, x :: xs)
      }
    go(count, rng, List())
  }

  type Rand[+A] = RNG => (A, RNG)
//  type Rand[A] = State[RNG, A]

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  implicit class RandOps[A](ra: Rand[A]) {
    def flatMap[B](f: A => Rand[B]): Rand[B] = RNG.flatMap(ra)(f)
    def map[B](f: A => B): Rand[B] = RNG.map(ra)(f)
  }

//  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
//    rng => {
//      val (a, rng2) = s(rng)
//      (f(a), rng2)
//    }

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)

  // Exercise 6.5
  val double: Rand[Double] = map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

  // Exercise 6.6
//  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
//    val (a, rng1) = ra(rng)
//    val (b, rng2) = rb(rng1)
//    (f(a, b), rng2)
//  }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] = both(int, double)

  val randDoubleInt: Rand[(Double, Int)] = both(double, int)

  // Exercise 6.7 - Stretch
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((ra: Rand[A], b: Rand[List[A]]) => map2(ra, b)(_ :: _))

  // Exercise 6.8
  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, r) = f(rng)
    g(a)(r)
  }

  // Exercise 6.8b
  def nonNegativeLessThan(n: Int): Rand[Int] = {
    flatMap(nonNegativeInt)(i => {
      val mod = i % n
      if (i + (n - 1) - mod >= n) unit(mod) else nonNegativeLessThan(n)
    })
  }

  // Exercise 6.9
  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(a => unit(f(a)))

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    flatMap(ra) { a =>
      flatMap(rb) { b =>
        unit(f(a, b))
      }
    }
  }

//  def rollDie: Rand[Int] = nonNegativeLessThan(6)
  def rollDie: Rand[Int] = map(nonNegativeLessThan(6))(_ + 1)


}
