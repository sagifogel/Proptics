package proptics.syntax

import cats.Functor

import proptics.ALens_

trait ALensSyntax {
  implicit def aLensSequenceOps[F[_], S, T, A](aLens: ALens_[S, T, F[A], A]): ALensSequenceOps[F, S, T, A] = ALensSequenceOps(aLens)
}

final case class ALensSequenceOps[F[_], S, T, A](private val lens: ALens_[S, T, F[A], A]) extends AnyVal {
  /** invert a structure of S containing F[A] to F[T], a structure T containing A's inside an Applicative Functor */
  def sequence(s: S)(implicit ev: Functor[F]): F[T] = lens.traverse(s)(identity)
}
