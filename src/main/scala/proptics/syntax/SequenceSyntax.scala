package proptics.syntax

import cats.kernel.Monoid
import cats.{Applicative, Functor}
import proptics._

object SequenceSyntax {
  implicit class TraversalSequenceOps[F[_], S, T, A](val traversal: Traversal_[S, T, F[A], A]) extends AnyVal {
    def sequence(s: S)(implicit ev: Applicative[F]): F[T] = traversal.traverse(s)(identity)
  }

  implicit class PrismSequenceOps[F[_], S, T, A](val prism: Prism_[S, T, F[A], A]) extends AnyVal {
    def sequence(s: S)(implicit ev: Applicative[F]): F[T] = prism.traverse(s)(identity)
  }

  implicit class LensSequenceOps[F[_], S, T, A](val lens: Lens_[S, T, F[A], A]) extends AnyVal {
    def sequence(s: S)(implicit ev: Functor[F]): F[T] = lens.traverse(s)(identity)
  }

  implicit class IsoSequenceOps[F[_], S, T, A](val iso: Iso_[S, T, F[A], A]) extends AnyVal {
    def sequence(s: S)(implicit ev: Applicative[F]): F[T] = iso.traverse(s)(identity)
  }

  implicit class IndexedTraversalSequenceOps[F[_], I, S, T, A](val iso: IndexedTraversal[I, S, T, F[A], A]) extends AnyVal {
    def sequence(s: S)(implicit ev: Applicative[F]): F[T] = iso.traverse(s)(_._2)
  }

  implicit class IndexedLensSequenceOps[F[_], I, S, T, A](val indexedLens: IndexedLens[I, S, T, F[A], A]) extends AnyVal {
    def sequence(s: S)(implicit ev: Applicative[F]): F[T] = indexedLens.traverse(s)(_._2)
  }

  implicit class GrateSequenceOps[F[_], I, S, T, A](val grate: Grate[S, T, F[A], A]) extends AnyVal {
    def sequence(s: S)(implicit ev0: Applicative[F], ev1: Monoid[F[A]]): F[T] = grate.traverse(s)(identity)
  }

  implicit class ATraversalSequenceOps[F[_], I, S, T, A](val grate: ATraversal_[S, T, F[A], A]) extends AnyVal {
    def sequence(s: S)(implicit ev0: Applicative[F]): F[T] = grate.traverse(s)(identity)
  }

  implicit class APrismSequenceOps[F[_], I, S, T, A](val prism: APrism[S, T, F[A], A]) extends AnyVal {
    def sequence(s: S)(implicit ev0: Applicative[F]): F[T] = prism.traverse(s)(identity)
  }

  implicit class AnIsoSequenceOps[F[_], I, S, T, A](val iso: AnIso_[S, T, F[A], A]) extends AnyVal {
    def sequence(s: S)(implicit ev0: Applicative[F]): F[T] = iso.traverse(s)(identity)
  }

  implicit class AnIndexedLensSequenceOps[F[_], I, S, T, A](val indexedLens: AnIndexedLens[I, S, T, F[A], A]) extends AnyVal {
    def sequence(s: S)(implicit ev: Applicative[F]): F[T] = indexedLens.traverse(s)(_._2)
  }

  implicit class ALensSequenceOps[F[_], S, T, A](val lens: ALens_[S, T, F[A], A]) extends AnyVal {
    def sequence(s: S)(implicit ev: Functor[F]): F[T] = lens.traverse(s)(identity)
  }

  implicit class AGrateSequenceOps[F[_], I, S, T, A](val grate: AGrate[S, T, F[A], A]) extends AnyVal {
    def sequence(s: S)(implicit ev0: Applicative[F], ev1: Monoid[F[A]]): F[T] = grate.traverse(s)(identity)
  }
}
