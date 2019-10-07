package mio

final case class EitherT[F[_]: Monad, E, A](value: F[Either[E, A]]) {
    def map[B](f: A => B): EitherT[F, E, B] = EitherT(value.map(_.map(f)))
    def flatMap[B](f: A => EitherT[F, E, B]): EitherT[F, E, B] = 
        EitherT(value.flatMap({
            case Left(value) => Monad[F].pure(Left(value))
            case Right(value) => f(value).value
        }))
}

object EitherT {

    implicit def monadForEitherT[F[_]: Monad, E]: Monad[EitherT[F, E, ?]] = 
        new Monad[EitherT[F, E, ?]] {
            def flatMap[A, B](fa: EitherT[F,E,A])(g: A => EitherT[F,E,B]): EitherT[F,E,B] = fa.flatMap(g)
            def map[A, B](fa: EitherT[F,E,A])(g: A => B): EitherT[F,E,B] = fa.map(g)
            def pure[A](a: A): EitherT[F,E,A] = EitherT(Monad[F].pure(Right(a)))
        }

    def liftF[F[_]: Monad, E, A](fa: F[A]): EitherT[F, E, A] = EitherT(fa.map(Right(_)))

    def fromEither[F[_]: Monad, E, A](either: Either[E, A]): EitherT[F, E, A] =  EitherT(Monad[F].pure(either))
}