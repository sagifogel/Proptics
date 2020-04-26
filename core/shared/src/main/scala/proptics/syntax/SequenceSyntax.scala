package proptics.syntax

import cats.{Applicative, Functor}
import proptics._

object SequenceSyntax {
  implicit class IndexedTraversalSequenceOps[F[_], I, S, T, A](val iso: IndexedTraversal_[I, S, T, F[A], A]) extends AnyVal {
    def sequence(s: S)(implicit ev: Applicative[F]): F[T] = iso.traverse(s)(_._2)
  }

  implicit class IndexedLensSequenceOps[F[_], I, S, T, A](val indexedLens: IndexedLens_[I, S, T, F[A], A]) extends AnyVal {
    def sequence(s: S)(implicit ev: Applicative[F]): F[T] = indexedLens.traverse(s)(_._2)
  }

  implicit class ATraversalSequenceOps[F[_], I, S, T, A](val grate: ATraversal_[S, T, F[A], A]) extends AnyVal {
    def sequence(s: S)(implicit ev0: Applicative[F]): F[T] = grate.traverse(s)(identity)
  }

  implicit class APrismSequenceOps[F[_], I, S, T, A](val prism: APrism_[S, T, F[A], A]) extends AnyVal {
    def sequence(s: S)(implicit ev0: Applicative[F]): F[T] = prism.traverse(s)(identity)
  }

  implicit class AnIndexedLensSequenceOps[F[_], I, S, T, A](val indexedLens: AnIndexedLens_[I, S, T, F[A], A]) extends AnyVal {
    def sequence(s: S)(implicit ev: Applicative[F]): F[T] = indexedLens.traverse(s)(_._2)
  }
}
