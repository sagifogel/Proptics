package proptics.syntax

import proptics.indices.FunctorWithIndex

trait FunctorWithIndexSyntax {
  implicit final def functorWithIndexOps[F[_], A](fa: F[A]): FunctorWithIndexOps[F, A] = FunctorWithIndexOps[F, A](fa)
}

final case class FunctorWithIndexOps[F[_], A](private val fa: F[A]) extends AnyVal {
  def map[B](f: A => B)(implicit ev: FunctorWithIndex[F, _]): F[B] =
    ev.map(fa)(f)

  def mapWithIndex[I, B](f: (A, I) => B)(implicit ev: FunctorWithIndex[F, I]): F[B] =
    ev.mapWithIndex(f)(fa)
}
