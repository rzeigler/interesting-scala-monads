package mio

trait MonadState[F[_], S] {
  def get: F[S]
  def set(s: S): F[Unit]
  def modify(f: S => S)(implicit M: Monad[F]): F[S] =
    get.map(f).flatMap(set).flatMap(_ => get)
}

object MonadState {
  def apply[F[_], S](implicit instance: MonadState[F, S]): MonadState[F, S] = instance
  
  implicit def monadStateForState[S]: MonadState[State[S, ?], S] =
    new MonadState[State[S, ?], S] {
      def get: State[S, S] = State.get
      def set(s: S): State[S, Unit] = State.set(s)
    }

  implicit def monadStateForStateT[F[_]: Monad, S]
      : MonadState[StateT[F, S, ?], S] = new MonadState[StateT[F, S, ?], S] {
    def get: StateT[F, S, S] = StateT.get
    def set(s: S): StateT[F, S, Unit] = StateT.set(s)
  }

  implicit def deriveMonadStateForEitherT[F[_]: Monad, S, E](
      implicit MS: MonadState[F, S]
  ): MonadState[EitherT[F, E, ?], S] =
    new MonadState[EitherT[F, E, ?], S] {
      def get: EitherT[F, E, S] = EitherT.liftF(MS.get)
      def set(s: S): EitherT[F, E, Unit] = EitherT.liftF(MS.set(s))
    }
}
