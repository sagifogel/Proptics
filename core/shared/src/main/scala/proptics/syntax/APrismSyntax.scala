package proptics.syntax

import cats.Applicative
import proptics.{APrism, APrism_, Prism}

trait APrismSyntax {
  implicit def asPrismOps[S, A](aPrism: APrism[S, A]) = AsPrismOps(aPrism)

  implicit def aPrismSequenceOps[F[_], S, T, A](aPrism: APrism_[S, T, F[A], A]) = APrismSequenceOps(aPrism)
}

final case class AsPrismOps[S, A](private val prism: APrism[S, A]) extends AnyVal {
  def asPrism: Prism[S, A] = prism.asPrism_
}

final case class APrismSequenceOps[F[_], S, T, A](private val prism: APrism_[S, T, F[A], A]) extends AnyVal {
  def sequence(s: S)(implicit ev0: Applicative[F]): F[T] = prism.traverse(s)(identity)
}
