package mio

trait MonadError[F[_], E] {
  def raiseError[A](e: E): F[A]
  def recoverWith[A](fa: F[A])(f: E => F[A]): F[A]
}

object MonadError {
  implicit def monadErrorForEither[E]: MonadError[Either[E, ?], E] =
    new MonadError[Either[E, ?], E] {
      def raiseError[A](e: E): Either[E, A] = Left(e)
      def recoverWith[A](fa: Either[E, A])(f: E => Either[E, A]): Either[E, A] =
        fa.fold(f, Right.apply)
    }

  implicit def monadErrorForEitherT[F[_]: Monad, E]
      : MonadError[EitherT[F, E, ?], E] = new MonadError[EitherT[F, E, ?], E] {
    def raiseError[A](e: E): EitherT[F, E, A] = EitherT.fromEither(Left(e))
    def recoverWith[A](
        fa: EitherT[F, E, A]
    )(f: E => EitherT[F, E, A]): EitherT[F, E, A] =
      EitherT(fa.value.flatMap(_ match {
        case Left(value)  => f(value).value
        case Right(value) => Monad[F].pure(Right(value))
      }))
  }

  implicit def liftMonadErrorThroughStateT[F[_]: Monad, E, S](
      implicit ME: MonadError[F, E]
  ): MonadError[StateT[F, S, ?], E] =
    new MonadError[StateT[F, S, ?], E] {
      def raiseError[A](e: E): StateT[F, S, A] = StateT.liftF(ME.raiseError(e))
      def recoverWith[A](fa: StateT[F, S, A])(
          f: E => StateT[F, S, A]
      ): StateT[F, S, A] = {
        StateT((s) => {
            ME.recoverWith(fa.run(s))(e => f(e).run(s))
        })
      }
    }
}
