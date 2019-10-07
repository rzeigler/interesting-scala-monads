package mio
import scala.collection.immutable.Nil

trait Traverse[F[_]] {
    // When I say monad, I really just mean Applicative
    def traverse[G[_]: Monad, A, B](fa: F[A])(f: A => G[B]): G[F[B]]
}

object Traverse {
    implicit val traverseForOption: Traverse[Option] = new Traverse[Option] {
        def traverse[G[_]: Monad, A, B](fa: Option[A])(f: A => G[B]): G[Option[B]] = 
            fa match {
                case None => Monad[G].pure(None)
                case Some(value) => Monad[G].map(f(value))(Some.apply)
            }
    }

    implicit val traverseForList: Traverse[List] = new Traverse[List] {
        def traverse[G[_]: Monad, A, B](fa: List[A])(f: A => G[B]): G[List[B]] = 
            fa.foldRight(Monad[G].pure(List.empty[B]))((a, gb) => 
                Monad[G].ap(f(a).map(a => (b: List[B]) => a :: b))(gb)
            )
            // fa match {
            //     case Nil => Monad[G].pure(Nil)
            //     case head :: tl => 
            //         val gb: G[B] = f(head)
            //         val ghToB = gb.map(gh => (bs: List[B]) =>  gh :: bs)
            //         Monad[G].ap(ghToB)(traverse(tl)(f))
            // }
    }
}