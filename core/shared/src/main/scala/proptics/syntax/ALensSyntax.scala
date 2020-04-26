package proptics.syntax

import cats.Functor
import proptics.{ALens, ALens_, Lens}

trait ALensSyntax {
  implicit def aLensAsLensOps[F[_], S, T, A](aLens: ALens_[S, T, F[A], A]) = ALensSequenceOps(aLens)

  implicit def aLensSequenceOps[F[_], S, T, A](aLens: ALens_[S, T, F[A], A]) = ALensSequenceOps(aLens)
}

final case class ALensAsLensOps[I, S, A](private val aLens: ALens[S, A]) extends AnyVal {
  def asLens: Lens[S, A] = aLens.asLens_
}

final case class ALensSequenceOps[F[_], S, T, A](private val lens: ALens_[S, T, F[A], A]) extends AnyVal {
  def sequence(s: S)(implicit ev: Functor[F]): F[T] = lens.traverse(s)(identity)
}
