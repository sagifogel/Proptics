package proptics.syntax.applied

import cats.{Foldable, Traverse}

import proptics.applied.{AppliedAffineTraversal_, AppliedFold_, AppliedTraversal_}
import proptics.{AffineTraversal_, Fold, Traversal}

trait AppliedAffineTraversalSyntax {
  implicit final def appliedAffineTraversalOps[F[_], S, T, A](traversal: AppliedAffineTraversal_[S, T, F[A], F[A]]): AppliedAffineTraversalOps[F, S, T, A] =
    AppliedAffineTraversalOps(traversal)
}

final case class AppliedAffineTraversalOps[F[_], S, T, A](private val appliedAffineTraversal: AppliedAffineTraversal_[S, T, F[A], F[A]]) extends AnyVal {
  def value: S = appliedAffineTraversal.value
  def optic: AffineTraversal_[S, T, F[A], F[A]] = appliedAffineTraversal.optic

  /** compose this [[AffineTraversal_]] with a [[Traversal]], having this [[AffineTraversal_]] applied first */
  def andThenTraverse(implicit ev: Traverse[F]): AppliedTraversal_[S, T, A, A] =
    AppliedTraversal_(value, optic.andThen(Traversal.fromTraverse[F, A]))

  /** compose this [[AffineTraversal_]] with a [[Fold]] having this [[AffineTraversal_]] applied first */
  def andThenFold(implicit ev: Foldable[F]): AppliedFold_[S, T, A, A] =
    AppliedFold_(appliedAffineTraversal.value, appliedAffineTraversal.optic.andThen(Fold.fromFoldable[F, A]))
}
