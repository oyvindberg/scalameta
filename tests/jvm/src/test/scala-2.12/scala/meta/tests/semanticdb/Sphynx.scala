package scala.meta.tests.semanticdb

object Sphynx {

  trait Functor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
    def lift[A, B](f: A => B): F[A] => F[B] = fa => map(fa)(f)
  }

  trait Apply[F[_]] extends Functor[F] {
    def ap[A, B](ff: F[A => B])(fa: F[A]): F[B]
  }

  trait Applicative[F[_]] extends Apply[F] {
    def pure[A](a: A): F[A]
    def map[A, B](fa: F[A])(f: A => B): F[B] = ap(pure(f))(fa)
  }

  trait FlatMap[F[_]] extends Apply[F] {
    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
  }

  trait Monad[F[_]] extends FlatMap[F] with Applicative[F]

  implicit class ApplicativeIdOps[A](val a: A) extends AnyVal {
    def pure[F[_]](implicit F: Applicative[F]): F[A] = F.pure(a)
  }

  implicit class ToFunctorOps[F[_], A](target: F[A])(implicit tc: Functor[F]) {
    def map[B](f: scala.Function1[A, B]): F[B] = tc.map(target)(f)
  }

  implicit class ToFlatMapOps[F[_], C](target: F[C])(implicit tc: FlatMap[F]) {
    def flatMap[B](f: scala.Function1[C, F[B]]): F[B] = tc.flatMap(target)(f)
  }
}
