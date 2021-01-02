package proptics.indices

import scala.annotation.implicitNotFound

import cats.Functor

@implicitNotFound("Could not find an instance of FunctorWithIndex[${F}, ${I}]")
trait FunctorWithIndex[F[_], I] extends Functor[F] {
  def mapWithIndex[A, B](f: (A, I) => B)(fa: F[A]): F[B]

  override def map[A, B](fa: F[A])(f: A => B): F[B] = mapWithIndex[A, B]((a, _) => f(a))(fa)
}

object FunctorWithIndex {
  /** summon an instance of [[FunctorWithIndex]] for `F` */
  @inline def apply[F[_], I](implicit instance: FunctorWithIndex[F, I]): FunctorWithIndex[F, I] = instance
}
