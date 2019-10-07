package mio
import scala.io.StdIn

trait MonadIO[F[_]] {
    def liftIO[A](ma: MIO[A]): F[A]
}

object MonadIO {
    def apply[F[_]](implicit instance: MonadIO[F]): MonadIO[F] = instance

    implicit val monadIoForMIO: MonadIO[MIO] = new MonadIO[MIO] {
        def liftIO[A](ma: MIO[A]): MIO[A] = ma
    }

    implicit def monadIoForEitherT[F[_]: Monad: MonadIO, E]: MonadIO[EitherT[F, E, ?]] = new MonadIO[EitherT[F, E, ?]] {
        def liftIO[A](ma: MIO[A]): EitherT[F, E, A] = EitherT.liftF(implicitly[MonadIO[F]].liftIO(ma))
    }

    implicit def monadIoForStateT[F[_]: Monad: MonadIO, S]: MonadIO[StateT[F, S, ?]] = new MonadIO[StateT[F, S, ?]] {
        def liftIO[A](ma: MIO[A]): StateT[F,S,A] = StateT.liftF(implicitly[MonadIO[F]].liftIO(ma))
    }

    def getStrLn[F[_]: MonadIO]: F[String] = implicitly[MonadIO[F]].liftIO(MIO.defer(StdIn.readLine()))
    def putStrLn[F[_]: MonadIO](s: String): F[Unit] = implicitly[MonadIO[F]].liftIO(MIO.defer(println(s)))
}