package mio

trait TraverseFilter[F[_]] {
  def filterA[G[_]: Monad, A](fa: F[A])(f: A => G[Boolean]): G[F[A]]
}

object TraverseFilter {
  implicit val traverseFilterForOption: TraverseFilter[Option] =
    new TraverseFilter[Option] {
      def filterA[G[_]: Monad, A](
          fa: Option[A]
      )(f: A => G[Boolean]): G[Option[A]] =
        fa match {
          case None        => Monad[G].pure(None)
          case Some(value) => f(value).map(if (_) Some(value) else None)
        }
    }

  implicit val traverseFilterForList: TraverseFilter[List] =
    new TraverseFilter[List] {
      def filterA[G[_]: Monad, A](fa: List[A])(f: A => G[Boolean]): G[List[A]] =
        fa.foldRight(Monad[G].pure(List.empty[A]))(
          (a, gb) =>{
            val consIf = f(a).map(bool => (rest: List[A]) => if (bool) a :: rest else rest)
             Monad[G].ap(consIf)(gb)
        })
    }
}
