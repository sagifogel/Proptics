package proptics.syntax

import cats.{Applicative, Bifunctor, Contravariant, Functor}

import proptics.{Iso, Iso_}

trait IsoSyntax {
  implicit def isoSequenceOps[F[_], S, T, A](iso: Iso_[S, T, F[A], A]): IsoSequenceOps[F, S, T, A] = IsoSequenceOps(iso)

  implicit def isoFunctorOps[S, A](iso: Iso[S, A]): IsoFunctorOps[S, A] = IsoFunctorOps(iso)
}

final case class IsoFunctorOps[S, A](iso: Iso[S, A]) extends AnyVal {
  def map[F[_]: Functor]: Iso[F[S], F[A]] = Iso.map[F](iso)

  def bimap[F[_, _]: Bifunctor]: Iso[F[S, S], F[A, A]] = Iso.bimap[F](iso)

  def contramap[F[_]: Contravariant]: Iso[F[A], F[S]] = Iso.contramap[F](iso)
}

final case class IsoSequenceOps[F[_], S, T, A](private val iso: Iso_[S, T, F[A], A]) extends AnyVal {
  /** invert a structure of S containing F[A] to F[T], a structure T containing A's inside an Applicative Functor */
  def sequence(s: S)(implicit ev: Applicative[F]): F[T] = iso.traverse(s)(identity)
}
