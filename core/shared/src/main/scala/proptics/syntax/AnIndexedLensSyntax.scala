package proptics.syntax

import cats.Applicative

import proptics.AnIndexedLens_

trait AnIndexedLensSyntax {
  implicit def anIndexedLensSequenceOps[F[_], I, S, T, A](anIndexedLens: AnIndexedLens_[I, S, T, F[A], A]): AnIndexedLensSequenceOps[F, I, S, T, A] =
    AnIndexedLensSequenceOps(anIndexedLens)
}

final case class AnIndexedLensSequenceOps[F[_], I, S, T, A](private val anIndexedLens: AnIndexedLens_[I, S, T, F[A], A]) extends AnyVal {
  /** invert a structure of S containing F[(I, A)] to F[T], a structure T containing A's inside an Applicative Functor */
  def sequence(s: S)(implicit ev: Applicative[F]): F[T] = anIndexedLens.traverse(s)(_._2)
}
