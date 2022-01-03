package proptics.syntax.applied

import cats.{Applicative, Foldable, Traverse}

import proptics.applied.{AppliedFold_, AppliedLens_, AppliedTraversal_}
import proptics.std.tuple.{_1P, _2P}
import proptics.{Fold, Lens_, Traversal}

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

final case class AppliedLensTraversalOps[F[_], S, T, A](private val appliedLens: AppliedLens_[S, T, F[A], F[A]]) extends AnyVal {
  def value: S = appliedLens.value
  def optic: Lens_[S, T, F[A], F[A]] = appliedLens.optic

  /** compose this [[AppliedLens_]] with a [[proptics.Fold]] having this [[AppliedLens_]] applied first */
  def andThenFold(implicit ev: Foldable[F]): AppliedFold_[S, T, A, A] =
    AppliedFold_(appliedLens.value, appliedLens.optic.andThen(Fold.fromFoldable[F, A]))

  /** compose this [[AppliedLens_]] with a [[Traversal]], having this [[AppliedLens_]] applied first */
  def andThenTraverse(implicit ev: Traverse[F]): AppliedTraversal_[S, T, A, A] =
    AppliedTraversal_(value, optic.andThen(Traversal.fromTraverse[F, A]))
}

final case class AppliedPolyLensOps[S, T, F[_], A](private val appliedLens: AppliedLens_[S, T, F[A], A]) extends AnyVal {
  /** invert a structure of S containing F[A] to F[T], a structure T containing A's inside an Applicative Functor */
  def sequence(implicit ev: Applicative[F]): F[T] =
    appliedLens.optic.traverse[F](appliedLens.value)(identity)
}
