package proptics.syntax

import cats.{Applicative, Functor}
import proptics.{Iso, Lens, Prism, Traversal}

object SequenceSyntax {
  implicit class TraversalSequenceOps[F[_], S, T, A](val traversal: Traversal[S, T, F[A], A]) extends AnyVal {
    def sequence(s: S)(implicit ev: Applicative[F]): F[T] = traversal.traverse(s)(identity)
  }

  implicit class PrismSequenceOps[F[_], S, T, A](val prism: Prism[S, T, F[A], A]) extends AnyVal {
    def sequence(s: S)(implicit ev: Applicative[F]): F[T] = prism.traverse(s)(identity)
  }

  implicit class LensSequenceOps[F[_], S, T, A](val lens: Lens[S, T, F[A], A]) extends AnyVal {
    def sequence(s: S)(implicit ev: Functor[F]): F[T] = lens.traverse(s)(identity)
  }

  implicit class IsoSequenceOps[F[_], S, T, A](val iso: Iso[S, T, F[A], A]) extends AnyVal {
    def sequence(s: S)(implicit ev: Applicative[F]): F[T] = iso.traverse(s)(identity)
  }

}
