package mio

trait MIOApp {
    def run(args: List[String]): MIO[Unit]

    def main(args: Array[String]): Unit = {
        new Runtime(run(args.toList)).unsafeRunSync
    }
}