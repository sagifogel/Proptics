package proptics.syntax

import cats.Functor

import proptics.Lens_

trait LensSyntax {
  implicit def lensSequenceOps[F[_], S, T, A](lens: Lens_[S, T, F[A], A]): LensSequenceOps[F, S, T, A] = LensSequenceOps(lens)
}

final case class LensSequenceOps[F[_], S, T, A](private val lens: Lens_[S, T, F[A], A]) extends AnyVal {
  def sequence(s: S)(implicit ev: Functor[F]): F[T] = lens.traverse(s)(identity)
}
