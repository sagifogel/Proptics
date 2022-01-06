package proptics.syntax.applied

import proptics.typeclass.Empty
import proptics.{AppliedAffineTraversal, AppliedFold, AppliedLens, AppliedTraversal}

trait AppliedEmptySyntax {
  implicit final def appliedLensEmptyOps[S, T](appliedLens: AppliedLens[S, T]): AppliedLensEmptyOps[S, T] =
    AppliedLensEmptyOps(appliedLens)

  implicit final def appliedFoldEmptyOps[S, T](appliedFold: AppliedFold[S, T]): AppliedFoldEmptyOps[S, T] =
    AppliedFoldEmptyOps(appliedFold)

  implicit final def appliedAffineTraversalEmptyOps[S, T](appliedAffineTraversal: AppliedAffineTraversal[S, T]): AppliedAffineTraversalEmptyOps[S, T] =
    AppliedAffineTraversalEmptyOps(appliedAffineTraversal)

  implicit final def appliedTraversalEmptyOps[S, T](appliedTraversal: AppliedTraversal[S, T]): AppliedTraversalEmptyOps[S, T] =
    AppliedTraversalEmptyOps(appliedTraversal)
}

case class AppliedLensEmptyOps[S, T](private val appliedLens: AppliedLens[S, T]) extends AnyVal {
  /** provide a way to know if a structure `T` is empty */
  def empty(implicit ev: Empty[T]): AppliedAffineTraversal[S, Unit] = appliedLens.andThen(ev.empty)
}

case class AppliedFoldEmptyOps[S, T](private val appliedFold: AppliedFold[S, T]) extends AnyVal {
  /** provide a way to know if a structure `T` is empty */
  def empty(implicit ev: Empty[T]): AppliedFold[S, Unit] = appliedFold.andThen(ev.empty)
}

case class AppliedAffineTraversalEmptyOps[S, T](private val appliedAffineTraversal: AppliedAffineTraversal[S, T]) extends AnyVal {
  /** provide a way to know if a structure `T` is empty */
  def empty(implicit ev: Empty[T]): AppliedAffineTraversal[S, Unit] = appliedAffineTraversal.andThen(ev.empty)
}

case class AppliedTraversalEmptyOps[S, T](private val appliedTraversal: AppliedTraversal[S, T]) extends AnyVal {
  /** provide a way to know if a structure `T` is empty */
  def empty(implicit ev: Empty[T]): AppliedTraversal[S, Unit] = appliedTraversal.andThen(ev.empty)
}
