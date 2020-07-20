package com.collinkueter.exercises.chapter6

case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] = flatMap(a => State.unit(f(a)))

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(s => {
      val (a, s1) = run(s)
      f(a).run(s1)
    })

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))
}

object State {
  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] =
    sas.foldRight(unit[S, List[A]](List[A]()))((ra: State[S, A], b: State[S, List[A]]) => ra.map2(b)(_ :: _))

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] =
    for {
      s <- get
      _ <- set(f(s))
    } yield ()
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input
case class Machine(locked: Boolean, candies: Int, coins: Int)

object CandyMachine {
  import State._

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    val list: List[State[Machine, Unit]] = inputs.map(
      i =>
        modify(
          (m: Machine) =>
            (i, m) match {
              case (Coin, Machine(true, candy, coins)) if candy > 0  => Machine(locked = false, candy, coins + 1)
              case (Turn, Machine(false, candy, coins)) if candy > 0 => Machine(locked = true, candy - 1, coins)
              case _                                                 => m
            }
        )
    )
    sequence(list).flatMap(_ => get).map(m => (m.coins, m.candies))
  }

  // Solution 1.
  //  for {
  //    _ <- sequence(inputs map (modify[Machine] _ compose update))
  //    s <- get
  //  } yield (s.coins, s.candies)

  // Solution 2.
//    for {
//      _ <- sequence(
//        inputs.map(
//          i =>
//            modify(
//              (m: Machine) =>
//                (i, m) match {
//                  case (Coin, Machine(true, candy, coin)) if candy > 0  => Machine(locked = false, candy, coin + 1)
//                  case (Turn, Machine(false, candy, coin)) if candy > 0 => Machine(locked = true, candy - 1, coin)
//                  case _                                                => m
//                }
//            )
//        )
//      )
//      m <- get
//    } yield (m.coins, m.candies)
}
