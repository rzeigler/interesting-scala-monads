package object mio extends MonadOps {
    import scala.io.StdIn
    def putStrLn(s: String): MIO[Unit] = MIO.defer(println(s))
    val getStrLn: MIO[String] = MIO.defer(StdIn.readLine())
    def random: MIO[Double] = MIO.defer(Math.random())
}
