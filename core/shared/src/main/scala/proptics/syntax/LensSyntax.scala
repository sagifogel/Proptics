package proptics.syntax

import cats.Functor

import proptics.Lens_

trait LensSyntax {
  implicit def lensSequenceOps[F[_], S, T, A](lens: Lens_[S, T, F[A], A]): LensSequenceOps[F, S, T, A] = LensSequenceOps(lens)
}

final case class LensSequenceOps[F[_], S, T, A](private val lens: Lens_[S, T, F[A], A]) extends AnyVal {
  /** invert a structure of S containing F[A] to F[T], a structure T containing A's inside an Applicative Functor */
  def sequence(s: S)(implicit ev: Functor[F]): F[T] = lens.traverse(s)(identity)
}
