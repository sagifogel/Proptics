package proptics.syntax

import cats.Applicative

import proptics.AnIso_

trait AnIsoSyntax {
  implicit def anIsoSequenceOps[F[_], S, T, A](iso: AnIso_[S, T, F[A], A]): AnIsoSequenceOps[F, S, T, A] = AnIsoSequenceOps(iso)
}

final case class AnIsoSequenceOps[F[_], S, T, A](private val iso: AnIso_[S, T, F[A], A]) extends AnyVal {
  /** invert a structure of S containing F[A] to F[T], a structure T containing A's inside an Applicative Functor */
  def sequence(s: S)(implicit ev0: Applicative[F]): F[T] = iso.traverse(s)(identity)
}
