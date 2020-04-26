package proptics.syntax

import cats.Applicative
import proptics.AnIso_

trait AnIsoSyntax {
  implicit def anIsoSequenceOps[F[_], I, S, T, A](iso: AnIso_[S, T, F[A], A]) = AnIsoSequenceOps(iso)
}

final case class AnIsoSequenceOps[F[_], I, S, T, A](private val iso: AnIso_[S, T, F[A], A]) extends AnyVal {
  def sequence(s: S)(implicit ev0: Applicative[F]): F[T] = iso.traverse(s)(identity)
}