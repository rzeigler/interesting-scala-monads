package mio
import mio.MIO.Pure
import mio.MIO.FlatMap

sealed abstract class MIO[A]

object MIO {
  final case class Pure[A](a: A) extends MIO[A]
  final case class Defer[A](a: () => A) extends MIO[A]
  final case class Async[A](f: ((A) => Unit) => Unit) extends MIO[A]
  final case class FlatMap[B, A](b: MIO[B], f: B => MIO[A]) extends MIO[A]

  implicit val monadForMIO: Monad[MIO] = new Monad[MIO] {
    def flatMap[A, B](fa: MIO[A])(g: A => MIO[B]): MIO[B] = FlatMap(fa, g)
    def map[A, B](fa: MIO[A])(g: A => B): MIO[B] =
      FlatMap(fa, (a: A) => Pure(g(a)))
    def pure[A](a: A): MIO[A] = Pure(a)
  }

  def pure[A](a: A): MIO[A] = Pure(a)
  def defer[A](a: => A): MIO[A] = Defer(() => a)
  def async[A](f: ((A) => Unit) => Unit): MIO[A] = Async(f)

  def shift[A](ma: MIO[A]): MIO[A] = 
    async(cb => {
        new Thread(new Runnable() {
            def run(): Unit = {
                val a = unsafeRunSync(ma)
                cb(a)
            }
        })
    })

  def sleep(ms: Int): MIO[Unit] = shift(defer(Thread.sleep(ms)))

  def unsafeRunAsync[A](mio: MIO[A], done: A => Unit): Unit = {
    mio match {
      case Pure(a)  => done(a)
      case Defer(a) => done(a())
      case Async(f) => f(done)
      case FlatMap(mib, f) =>
        unsafeRunAsync(mib, (b: Any) => unsafeRunAsync(f(b), done))
    }
  }

  def unsafeRunSync[A](mio: MIO[A]): A = {
    val gate = new Object()
    var done = false
    var slot: Option[A] = None
    unsafeRunAsync(mio, (a: A) => {
      slot = Some(a)
      gate.synchronized {
        done = true
        gate.notifyAll
      }
    })
    gate.synchronized {
      if (!done)
        gate.wait()
    }
    slot.get
  }

}
