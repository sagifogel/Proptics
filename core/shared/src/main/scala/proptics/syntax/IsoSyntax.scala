package proptics.syntax

import cats.Applicative
import proptics.{Iso, Iso_, Lens}

trait IsoSyntax {
  implicit def isoAsLensOps[I, S, A](iso: Iso[S, A]) = IsoAsLensOps(iso)

  implicit def isoSequenceOps[F[_], S, T, A](iso: Iso_[S, T, F[A], A]) = IsoSequenceOps(iso)
}

final case class IsoAsLensOps[I, S, A](private val iso: Iso[S, A]) extends AnyVal {
  def asLens: Lens[S, A] = iso.asLens_
}

final case class IsoSequenceOps[F[_], S, T, A](private val iso: Iso_[S, T, F[A], A]) extends AnyVal {
  def sequence(s: S)(implicit ev: Applicative[F]): F[T] = iso.traverse(s)(identity)
}