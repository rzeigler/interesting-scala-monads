package mio

trait Monad[F[_]] {
  def map[A, B](fa: F[A])(g: A => B): F[B]
  def flatMap[A, B](fa: F[A])(g: A => F[B]): F[B]
  def pure[A](a: A): F[A]
  final def ap[A, B](fab: F[A => B])(fa: F[A]): F[B] = flatMap(fab)(f => map(fa)(f))
  final def forever[A](fa: F[A]): F[A] = flatMap(fa)((a) => forever(fa))
}

object Monad {
  def apply[M[_]](implicit instance: Monad[M]): Monad[M] = instance

  implicit def monadForOption: Monad[Option] = new Monad[Option] {
    def flatMap[A, B](fa: Option[A])(g: A => Option[B]): Option[B] = fa.flatMap(g)
    def map[A, B](fa: Option[A])(g: A => B): Option[B] = fa.map(g)
    def pure[A](a: A): Option[A] = Some(a)
  }
}


trait MonadOps {
  final class MonadSyntax[M[_]: Monad, A](ma: M[A]) {
    def map[B](f: A => B): M[B] = implicitly[Monad[M]].map(ma)(f)
    def flatMap[B](g: A => M[B]): M[B] = implicitly[Monad[M]].flatMap(ma)(g)
    def pure[A](a: A): M[A] = implicitly[Monad[M]].pure(a)
    def forever: M[A] = implicitly[Monad[M]].forever(ma)
  }

  implicit def toMonadSyntax[M[_]: Monad, A](ma: M[A]) =
    new MonadSyntax[M, A](ma)
}
