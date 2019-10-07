package mio

trait ConsoleIO[F[_]] {
    def putStrLn(s: String): F[Unit]
    def getStrLn: F[String]
}

object ConsoleIO {
    implicit def forLiftIO[F[_]: MonadIO]: ConsoleIO[F] = new ConsoleIO[F] {
        val getStrLn: F[String] = MonadIO[F].liftIO(mio.getStrLn)
        def putStrLn(s: String): F[Unit] = MonadIO[F].liftIO(mio.putStrLn(s))
    }
}