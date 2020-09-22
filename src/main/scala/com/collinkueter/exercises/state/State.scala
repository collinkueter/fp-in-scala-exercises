package com.collinkueter.exercises.state

case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] = flatMap(a => State.unit(f(a)))

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(s => {
      val (a, s1) = run(s)
      f(a).run(s1)
    })

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] = flatMap(a => sb.map(b => f(a, b)))
}

object State {
  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def map2[S, A, B, C](as: State[S, A], bs: State[S, B])(f: (A, B) => C): State[S, C] =
    for {
      a <- as
      b <- bs
    } yield f(a, b)

  def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] =
    sas.foldRight(unit[S, List[A]](List[A]()))((ra: State[S, A], b: State[S, List[A]]) => ra.map2(b)(_ :: _))

  def sequence[S, A](fs: LazyList[State[S, A]]): State[S, LazyList[A]] =
    fs.foldRight[State[S, LazyList[A]]](unit(LazyList.empty[A]))(_.map2(_)(_ #:: _))

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] =
    for {
      s <- get
      _ <- set(f(s))
    } yield ()
}
