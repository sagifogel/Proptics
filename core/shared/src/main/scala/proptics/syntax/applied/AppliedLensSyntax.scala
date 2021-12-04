package proptics.syntax.applied

import cats.{Applicative, Id, catsInstancesForId}

import proptics.AppliedLens
import proptics.applied.AppliedLens_
import proptics.std.tuple.{_1P, _2P}

trait AppliedLensSyntax {
  implicit final def tuple2ToPolyAppliedLensOps[A, B](s: (A, B)): Tuple2ToPolyAppliedLensOps[A, B] = Tuple2ToPolyAppliedLensOps(s)

  implicit final def appliedMonoLensOps[F[_], A](appliedLens: AppliedLens[F[A], A]): AppliedMonoLensOps[F, A] = AppliedMonoLensOps(appliedLens)

  implicit final def appliedPolyLensOps[S, T, F[_], A](appliedLens: AppliedLens_[S, T, F[A], A]): AppliedPolyLensOps[S, T, F, A] = AppliedPolyLensOps(appliedLens)
}

case class Tuple2ToPolyAppliedLensOps[A, B](private val s: (A, B)) extends AnyVal {
  def first_[C]: AppliedLens_[(A, B), (C, B), A, C] = AppliedLens_(s, _1P[A, C, B])

  def second_[C]: AppliedLens_[(A, B), (A, C), B, C] = AppliedLens_(s, _2P[B, C, A])
}

final case class AppliedMonoLensOps[F[_], A](private val appliedLens: AppliedLens[F[A], A]) extends AnyVal {
  def sequence(implicit ev: Applicative[F]): F[A] =
    appliedLens.optic.traverse[Id](appliedLens.value)(identity)
}

final case class AppliedPolyLensOps[S, T, F[_], A](private val appliedLens: AppliedLens_[S, T, F[A], A]) extends AnyVal {
  def sequence(implicit ev: Applicative[F]): F[T] =
    appliedLens.optic.traverse[F](appliedLens.value)(identity)
}
