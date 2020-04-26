package proptics.syntax

import cats.Applicative
import proptics.{AnIndexedLens, AnIndexedLens_, IndexedLens, Lens}

trait AnIndexedLensSyntax {
  implicit def anIndexedLensAsOps[I, S, A](anIndexedLens: AnIndexedLens[I, S, A]) = AnIndexedLensAsOps(anIndexedLens)

  implicit def anIndexedLensSequenceOps[F[_], I, S, T, A](anIndexedLens: AnIndexedLens_[I, S, T, F[A], A]) = AnIndexedLensSequenceOps(anIndexedLens)
}

final case class AnIndexedLensAsOps[I, S, A](private val anIndexedLens: AnIndexedLens[I, S, A]) extends AnyVal {
  def asLens: Lens[S, A] = anIndexedLens.asLens_

  def asIndexedLens: IndexedLens[I, S, A] = anIndexedLens.asIndexedLens_
}

final case class AnIndexedLensSequenceOps[F[_], I, S, T, A](private val anIndexedLens: AnIndexedLens_[I, S, T, F[A], A]) extends AnyVal {
  def sequence(s: S)(implicit ev: Applicative[F]): F[T] = anIndexedLens.traverse(s)(_._2)
}
