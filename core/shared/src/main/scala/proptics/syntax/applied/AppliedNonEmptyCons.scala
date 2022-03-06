package proptics.syntax.applied

import proptics.applied.{AppliedIso, AppliedLens}
import proptics.typeclass.NonEmptyCons
import proptics.{AppliedAffineTraversal, _}

trait AppliedNonEmptyCons {
  implicit final def nonEmptyConsOps[S, H, T](s: S): NonEmptyConsOps[S, H, T] = NonEmptyConsOps(s)

  implicit final def appliedLensNonEmptyConsOps[S, T, H, A](appliedLens: AppliedLens[S, A]): AppliedLensNonEmptyConsOps[S, T, H, A] =
    AppliedLensNonEmptyConsOps(appliedLens)

  implicit final def appliedFoldNonEmptyConsOps[S, T, H, A](appliedFold: AppliedFold[S, A]): AppliedFoldNonEmptyConsOps[S, T, H, A] =
    AppliedFoldNonEmptyConsOps(appliedFold)

  implicit final def appliedAffineTraversalNonEmptyConsOps[S, T, H, A](appliedAffineTraversal: AppliedAffineTraversal[S, A]): AppliedAffineTraversalNonEmptyConsOps[S, T, H, A] =
    AppliedAffineTraversalNonEmptyConsOps(appliedAffineTraversal)

  implicit final def appliedTraversalNonEmptyConsOps[S, T, H, A](appliedTraversal: AppliedTraversal[S, A]): AppliedTraversalNonEmptyConsOps[S, T, H, A] =
    AppliedTraversalNonEmptyConsOps(appliedTraversal)
}

case class NonEmptyConsOps[S, H, T](private val s: S) extends AnyVal {
  /** splits the head and the tail of a data structure */
  def nonEmptyCons(implicit ev: NonEmptyCons[S, H, T]): AppliedIso[S, (H, T)] = AppliedIso[S, (H, T)](s, ev.nonEmptyCons)

  /** selects the first element of a data structure */
  def head(implicit ev: NonEmptyCons[S, H, T]): AppliedLens[S, H] = AppliedLens(s, ev.head)

  /** selects the tail of a data structure */
  def tail(implicit ev: NonEmptyCons[S, H, T]): AppliedLens[S, T] = AppliedLens(s, ev.tail)
}

case class AppliedLensNonEmptyConsOps[S, H, T, A](private val appliedLens: AppliedLens[S, A]) extends AnyVal {
  /** splits the head and the tail of a data structure */
  def nonEmptyCons(implicit ev: NonEmptyCons[A, H, T]): AppliedLens[S, (H, T)] = appliedLens.andThen(ev.nonEmptyCons)

  /** selects the first element of a data structure */
  def head(implicit ev: NonEmptyCons[A, H, T]): AppliedLens[S, H] = appliedLens.andThen(ev.head)

  /** selects the tail of a data structure */
  def tail(implicit ev: NonEmptyCons[A, H, T]): AppliedLens[S, T] = appliedLens.andThen(ev.tail)
}

case class AppliedFoldNonEmptyConsOps[S, H, T, A](private val appliedFold: AppliedFold[S, A]) extends AnyVal {
  /** splits the head and the tail of a data structure */
  def nonEmptyCons(implicit ev: NonEmptyCons[A, H, T]): AppliedFold[S, (H, T)] =
    appliedFold.andThen(ev.nonEmptyCons)

  /** selects the first element of a data structure */
  def head(implicit ev: NonEmptyCons[A, H, T]): AppliedFold[S, H] = appliedFold.andThen(ev.head)

  /** selects the tail of a data structure */
  def tail(implicit ev: NonEmptyCons[A, H, T]): AppliedFold[S, T] = appliedFold.andThen(ev.tail)
}

case class AppliedAffineTraversalNonEmptyConsOps[S, H, T, A](private val appliedAffineTraversal: AppliedAffineTraversal[S, A]) extends AnyVal {
  /** splits the head and the tail of a data structure */
  def nonEmptyCons(implicit ev: NonEmptyCons[A, H, T]): AppliedAffineTraversal[S, (H, T)] =
    appliedAffineTraversal.andThen(ev.nonEmptyCons)

  /** selects the first element of a data structure */
  def head(implicit ev: NonEmptyCons[A, H, T]): AppliedFold[S, H] = appliedAffineTraversal.andThen(ev.head)

  /** selects the tail of a data structure */
  def tail(implicit ev: NonEmptyCons[A, H, T]): AppliedFold[S, T] = appliedAffineTraversal.andThen(ev.tail)
}

case class AppliedTraversalNonEmptyConsOps[S, T, H, A](private val appliedTraversal: AppliedTraversal[S, A]) extends AnyVal {
  /** splits the head and the tail of a data structure */
  def nonEmptyCons(implicit ev: NonEmptyCons[A, H, T]): AppliedTraversal[S, (H, T)] = appliedTraversal.andThen(ev.nonEmptyCons)

  /** selects the first element of a data structure */
  def head(implicit ev: NonEmptyCons[A, H, T]): AppliedFold[S, H] = appliedTraversal.andThen(ev.head)

  /** selects the tail of a data structure */
  def tail(implicit ev: NonEmptyCons[A, H, T]): AppliedFold[S, T] = appliedTraversal.andThen(ev.tail)
}
