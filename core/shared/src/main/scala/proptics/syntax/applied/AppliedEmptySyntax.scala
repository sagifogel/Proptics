package proptics.syntax.applied

import proptics._
import proptics.typeclass.Empty

trait AppliedEmptySyntax {
  implicit final def appliedLensEmptyOps[S, T](appliedLens: AppliedLens[S, T]): AppliedLensEmptyOps[S, T] =
    AppliedLensEmptyOps(appliedLens)

  implicit final def appliedFoldEmptyOps[S, T](appliedFold: AppliedFold[S, T]): AppliedFoldEmptyOps[S, T] =
    AppliedFoldEmptyOps(appliedFold)

  implicit final def appliedAffineTraversalEmptyOps[S, T](appliedPrism: AppliedPrism[S, T]): AppliedPrismEmptyOps[S, T] =
    AppliedPrismEmptyOps(appliedPrism)

  implicit final def appliedAffineTraversalEmptyOps[S, T](appliedAffineTraversal: AppliedAffineTraversal[S, T]): AppliedAffineTraversalEmptyOps[S, T] =
    AppliedAffineTraversalEmptyOps(appliedAffineTraversal)

  implicit final def appliedTraversalEmptyOps[S, T](appliedTraversal: AppliedTraversal[S, T]): AppliedTraversalEmptyOps[S, T] =
    AppliedTraversalEmptyOps(appliedTraversal)
}

final case class AppliedLensEmptyOps[S, T](private val appliedLens: AppliedLens[S, T]) extends AnyVal {
  /** provide a way to know if a structure `T` is empty */
  def empty(implicit ev: Empty[T]): AppliedAffineTraversal[S, Unit] = appliedLens.andThen(ev.empty)
}

final case class AppliedFoldEmptyOps[S, T](private val appliedFold: AppliedFold[S, T]) extends AnyVal {
  /** provide a way to know if a structure `T` is empty */
  def empty(implicit ev: Empty[T]): AppliedFold[S, Unit] = appliedFold.andThen(ev.empty)
}

final case class AppliedPrismEmptyOps[S, T](private val appliedPrism: AppliedPrism[S, T]) extends AnyVal {
  /** provide a way to know if a structure `T` is empty */
  def empty(implicit ev: Empty[T]): AppliedPrism[S, Unit] = appliedPrism.andThen(ev.empty)
}

final case class AppliedAffineTraversalEmptyOps[S, T](private val appliedAffineTraversal: AppliedAffineTraversal[S, T]) extends AnyVal {
  /** provide a way to know if a structure `T` is empty */
  def empty(implicit ev: Empty[T]): AppliedAffineTraversal[S, Unit] = appliedAffineTraversal.andThen(ev.empty)
}

final case class AppliedTraversalEmptyOps[S, T](private val appliedTraversal: AppliedTraversal[S, T]) extends AnyVal {
  /** provide a way to know if a structure `T` is empty */
  def empty(implicit ev: Empty[T]): AppliedTraversal[S, Unit] = appliedTraversal.andThen(ev.empty)
}
