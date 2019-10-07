package mio
import mio.MIO.Async
import mio.MIO.Defer
import mio.MIO.FlatMap
import mio.MIO.Pure
import scala.collection.immutable.Nil

final class Runtime[A](ma: MIO[A]) {
    private var heapStack: List[Any => MIO[Any]] = List.empty
    private val gate = new Object()
    private var done = false
    private var slot: Option[A] = None

    def unsafeRunSync: A = {
        runloop(ma.asInstanceOf[MIO[Any]])
        gate.synchronized {
            if (!done)
              gate.wait()
          }
          slot.get
    }

    private def runloop(ma: MIO[Any]): Unit = {
        var current = ma
        while (current != null) {
            current match {
                case FlatMap(b, f) =>
                    heapStack = f :: heapStack
                    current = b
                case Defer(a) =>
                    val result = a()
                    current = step(result)
                case Pure(a) =>
                    current = step(a)
                case Async(f) => 
                    current = null
                    f((result) => runloop(MIO.pure(result)))
            }
        }
    }

    private def step(a: Any): MIO[Any] = 
        heapStack match {
            case head :: tl => 
                heapStack = tl
                head(a)
            case Nil => 
                gate.synchronized {
                    slot = Some(a.asInstanceOf[A])
                    done = true
                    gate.notifyAll
                }
                null
        }
}