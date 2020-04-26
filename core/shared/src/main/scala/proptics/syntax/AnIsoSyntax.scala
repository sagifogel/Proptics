package proptics.syntax

import cats.Applicative
import proptics.{AnIso, AnIso_, Iso}

trait AnIsoSyntax {
  implicit def asIsoOps[S, A](anIso: AnIso[S, A]) = AsIsoOps(anIso)

  implicit def anIsoSequenceOps[F[_], S, T, A](iso: AnIso_[S, T, F[A], A]) = AnIsoSequenceOps(iso)
}

final case class AnIsoSequenceOps[F[_], S, T, A](private val iso: AnIso_[S, T, F[A], A]) extends AnyVal {
  def sequence(s: S)(implicit ev0: Applicative[F]): F[T] = iso.traverse(s)(identity)
}

final case class AsIsoOps[S, A](private val anIso: AnIso[S, A]) extends AnyVal {
  def asIso: Iso[S, A] = anIso.asIso_
}