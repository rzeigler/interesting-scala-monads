package mio

object StateExample extends MIOApp {
  def run(args: List[String]): MIO[Unit] =
    execute.runA(0)

  def execute: StateT[MIO, Int, Unit] =
    Monad[({ type L[A] = StateT[MIO, Int, A] })#L].forever(step)

  def step: StateT[MIO, Int, Unit] =
    for {
      current <- StateT.get[MIO, Int]
      _ <- StateT.liftF(putStrLn(s"current: $current"))
      modify <- StateT.liftF(getStrLn)
      _ <- if (modify == "+") {
        StateT.modify[MIO, Int](_ + 1)
      } else if (modify == "-") {
        StateT.modify[MIO, Int](_ - 1)
      } else {
        StateT.liftF[MIO, Int, Unit](putStrLn("enter either +/-"))
      }
    } yield ()
}
