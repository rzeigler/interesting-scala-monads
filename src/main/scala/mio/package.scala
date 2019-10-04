package object mio extends MonadOps {
    def putStrLn(s: String): MIO[Unit] = MIO.defer(println(s))
    def random: MIO[Double] = MIO.defer(Math.random())
}
