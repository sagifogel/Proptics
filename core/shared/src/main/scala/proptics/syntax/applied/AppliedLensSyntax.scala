package proptics.syntax.applied

import cats.{Applicative, Id, catsInstancesForId}

import proptics.AppliedLens
import proptics.applied.AppliedLens_
import proptics.std.tuple.{_1P, _2P}

trait AppliedLensSyntax {
  implicit final def tuple2ToPolyAppliedLensOps[A, B](s: (A, B)): Tuple2ToPolyAppliedLensOps[A, B] = Tuple2ToPolyAppliedLensOps(s)

  implicit final def appliedPolyLensOps[S, T, F[_], A](appliedLens: AppliedLens_[S, T, F[A], A]): AppliedPolyLensOps[S, T, F, A] = AppliedPolyLensOps(appliedLens)
}

case class Tuple2ToPolyAppliedLensOps[A, B](private val s: (A, B)) extends AnyVal {
  /** select the first element of a tuple using polymorphic [[proptics.Lens_]] */
  def first_[C]: AppliedLens_[(A, B), (C, B), A, C] = AppliedLens_(s, _1P[A, C, B])

  /** select the second element of a tuple using polymorphic [[proptics.Lens_]] */
  def second_[C]: AppliedLens_[(A, B), (A, C), B, C] = AppliedLens_(s, _2P[B, C, A])
}

final case class AppliedPolyLensOps[S, T, F[_], A](private val appliedLens: AppliedLens_[S, T, F[A], A]) extends AnyVal {
  /** invert a structure of S containing F[A] to F[T], a structure T containing A's inside an Applicative Functor */
  def sequence(implicit ev: Applicative[F]): F[T] =
    appliedLens.optic.traverse[F](appliedLens.value)(identity)
}
