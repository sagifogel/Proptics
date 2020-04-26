package proptics.syntax

import cats.Applicative
import proptics.Iso_

trait IsoSyntax {
  implicit def isoSequenceOps[F[_], S, T, A](iso: Iso_[S, T, F[A], A]) = IsoSequenceOps(iso)
}

final case class IsoSequenceOps[F[_], S, T, A](private val iso: Iso_[S, T, F[A], A]) extends AnyVal {
  def sequence(s: S)(implicit ev: Applicative[F]): F[T] = iso.traverse(s)(identity)
}
