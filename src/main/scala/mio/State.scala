package mio

final case class State[S, A](run: (S) => (S, A)) {
  def runA(s: S): A = run(s)._2
}

object State {
  /**
   * We have to do some magic to make an instance for a State[S, _]
   * Think of State as a function at the type level that takes 2 concrete types as arguments
   * Here we are currying that function from (S, A) => State[S, A] to S => A => State[S, A]
   * because we need to define our monad on type constructor of the form A => F[A]
   * Applying S gives us the A => F[A] we want
   */
  trait CurryType[S] {
      type L[A] = State[S, A]
  }
  implicit def monadForState[S]: Monad[CurryType[S]#L] = {
      type F[A] = CurryType[S]#L[A]
      new Monad[F] {
        def map[A, B](fa: F[A])(g: A => B): F[B] = ???
        def flatMap[A, B](fa: F[A])(g: A => F[B]): F[B] = ???
        def pure[A](a: A): F[A] = ???
    }
  }
}
