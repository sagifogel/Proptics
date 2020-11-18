package proptics.syntax

import cats.Applicative

import proptics.IndexedLens_

trait IndexedLensSyntax {
  implicit def indexedLensSequenceOps[F[_], I, S, T, A](indexedLens: IndexedLens_[I, S, T, F[A], A]): IndexedLensSequenceOps[F, I, S, T, A] =
    IndexedLensSequenceOps(indexedLens)
}

final case class IndexedLensSequenceOps[F[_], I, S, T, A](private val indexedLens: IndexedLens_[I, S, T, F[A], A]) extends AnyVal {
  def sequence(s: S)(implicit ev: Applicative[F]): F[T] = indexedLens.traverse(s)(_._2)
}
