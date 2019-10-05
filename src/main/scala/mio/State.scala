package mio

final case class State[S, A](run: (S) => (S, A)) {
  def runA(s: S): A = run(s)._2
  def map[B](f: A => B): State[S, B] =
    State((s) => {
      val (s2, a) = run(s)
      (s2, f(a))
    })
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State((s) => {
      val (s2, a) = run(s)
      f(a).run(s2)
    })
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
      def map[A, B](fa: F[A])(g: A => B): F[B] = fa.map(g)
      def flatMap[A, B](fa: F[A])(g: A => F[B]): F[B] = fa.flatMap(g)
      def pure[A](a: A): F[A] = State(s => (s, a))
    }
  }

  def get[S]: State[S, S] = State(s => (s, s))
  def set[S](s: S): State[S, Unit] = State(_ => (s, ()))
  def modify[S](f: S => S): State[S, S] =
    get[S].map(f).flatMap(set[S]).flatMap(_ => get[S])
}
