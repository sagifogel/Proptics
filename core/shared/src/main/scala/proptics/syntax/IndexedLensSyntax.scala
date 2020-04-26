package proptics.syntax

import cats.Applicative
import proptics.{IndexedLens, IndexedLens_, Lens}

trait IndexedLensSyntax {
  implicit def indexedLensSequenceOps[F[_], I, S, T, A](indexedLens: IndexedLens_[I, S, T, F[A], A]) = IndexedLensSequenceOps(indexedLens)

  implicit def indexedLensAsOps[I, S, A](indexedLens: IndexedLens[I, S, A]) = IndexedLensAsOps(indexedLens)
}

final case class IndexedLensSequenceOps[F[_], I, S, T, A](private val indexedLens: IndexedLens_[I, S, T, F[A], A]) extends AnyVal {
  def sequence(s: S)(implicit ev: Applicative[F]): F[T] = indexedLens.traverse(s)(_._2)
}

final case class IndexedLensAsOps[I, S, A](private val indexedLens: IndexedLens[I, S, A]) extends AnyVal {
  def asLens: Lens[S, A] = indexedLens.asLens_
}
