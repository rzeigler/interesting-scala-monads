package mio

object StateExample extends MIOApp {
  type AppM[A] = EitherT[StateT[MIO, Int, ?], String, A]
  // StateT[EitherT[MIO, String, ?], Int, A]

  def run(args: List[String]): MIO[Unit] =
    execute.value.runA(76).map(_ => ())

  def execute: AppM[Unit] = 
    Monad[AppM].forever(step[AppM])

  def step[F[_]: Monad](implicit MS: MonadState[F, Int], ME: MonadError[F, String], IO: ConsoleIO[F]): F[Unit] =
    for {
      current <- MS.get
      _ <- IO.putStrLn(s"thermostate set to $current")
      update <- loopGetChange
      _ <- MS.modify(_ + update)
    } yield ()

  def loopGetChange[F[_]: Monad](implicit  ME: MonadError[F, String], IO: ConsoleIO[F]): F[Int] = 
    ME.recoverWith(getChange)(e => IO.putStrLn("invalid change").flatMap(_ => loopGetChange))

  def getChange[F[_]: Monad](implicit  ME: MonadError[F, String], IO: ConsoleIO[F]): F[Int] = 
    for {
      _ <- IO.putStrLn("enter a change:")
      in <- IO.getStrLn
      result <- if (in == "+") Monad[F].pure(1) else if (in == "-") Monad[F].pure(-1) else ME.raiseError("enter a valid change")
    } yield result
}
