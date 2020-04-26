package proptics.syntax

import cats.Applicative
import proptics.APrism_

trait APrismSyntax {
  implicit def aPrismSequenceOps[F[_], S, T, A](aPrism: APrism_[S, T, F[A], A]) = APrismSequenceOps(aPrism)
}

final case class APrismSequenceOps[F[_], S, T, A](private val prism: APrism_[S, T, F[A], A]) extends AnyVal {
  def sequence(s: S)(implicit ev0: Applicative[F]): F[T] = prism.traverse(s)(identity)
}
