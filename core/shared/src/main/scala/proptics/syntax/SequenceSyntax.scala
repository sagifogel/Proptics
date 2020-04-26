package proptics.syntax

import cats.Applicative
import proptics._

object SequenceSyntax {
  implicit class IndexedTraversalSequenceOps[F[_], I, S, T, A](val iso: IndexedTraversal_[I, S, T, F[A], A]) extends AnyVal {
    def sequence(s: S)(implicit ev: Applicative[F]): F[T] = iso.traverse(s)(_._2)
  }

  implicit class IndexedLensSequenceOps[F[_], I, S, T, A](val indexedLens: IndexedLens_[I, S, T, F[A], A]) extends AnyVal {
    def sequence(s: S)(implicit ev: Applicative[F]): F[T] = indexedLens.traverse(s)(_._2)
  }

  implicit class AnIndexedLensSequenceOps[F[_], I, S, T, A](val indexedLens: AnIndexedLens_[I, S, T, F[A], A]) extends AnyVal {
    def sequence(s: S)(implicit ev: Applicative[F]): F[T] = indexedLens.traverse(s)(_._2)
  }
}
