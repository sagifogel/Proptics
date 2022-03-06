package proptics.syntax.applied

import proptics._
import proptics.std.tuple._

trait AppliedTupleSyntax {
  implicit final def appliedLensSwapTuple2Ops[A, B, C](appliedLens: AppliedLens[A, (B, C)]): AppliedLensSwapTuple2Ops[A, B, C] =
    AppliedLensSwapTuple2Ops(appliedLens)

  implicit final def appliedPrismSwapTuple2Ops[A, B, C](appliedPrism: AppliedPrism[A, (B, C)]): AppliedPrismSwapTuple2Ops[A, B, C] =
    AppliedPrismSwapTuple2Ops(appliedPrism)

  implicit final def appliedAffineTraversalSwapTuple2Ops[A, B, C](appliedAffineTraversal: AppliedAffineTraversal[A, (B, C)]): AppliedAffineTraversalSwapTuple2Ops[A, B, C] =
    AppliedAffineTraversalSwapTuple2Ops(appliedAffineTraversal)

  implicit final def appliedFoldSwapTuple2Ops[A, B, C](appliedFold: AppliedFold[A, (B, C)]): AppliedFoldSwapTuple2Ops[A, B, C] =
    AppliedFoldSwapTuple2Ops(appliedFold)

  implicit final def appliedTraversalSwapTuple2Ops[A, B, C](appliedLens: AppliedTraversal[A, (B, C)]): AppliedTraversalSwapTuple2Ops[A, B, C] =
    AppliedTraversalSwapTuple2Ops(appliedLens)
}

final case class AppliedLensSwapTuple2Ops[A, B, C](private val appliedLens: AppliedLens[A, (B, C)]) extends AnyVal {
  /** swap the elements of a Tuple */
  def swap: AppliedLens[A, (C, B)] = appliedLens.andThen(swapTuple[B, C])
}

final case class AppliedPrismSwapTuple2Ops[A, B, C](private val appliedPrism: AppliedPrism[A, (B, C)]) extends AnyVal {
  /** swap the elements of a Tuple */
  def swap: AppliedPrism[A, (C, B)] = appliedPrism.andThen(swapTuple[B, C])
}

final case class AppliedAffineTraversalSwapTuple2Ops[A, B, C](private val appliedAffineTraversal: AppliedAffineTraversal[A, (B, C)]) extends AnyVal {
  /** swap the elements of a Tuple */
  def swap: AppliedAffineTraversal[A, (C, B)] = appliedAffineTraversal.andThen(swapTuple[B, C])
}

final case class AppliedFoldSwapTuple2Ops[A, B, C](private val appliedFold: AppliedFold[A, (B, C)]) extends AnyVal {
  /** swap the elements of a Tuple */
  def swap: AppliedFold[A, (C, B)] = appliedFold.andThen(swapTuple[B, C])
}

final case class AppliedTraversalSwapTuple2Ops[A, B, C](private val appliedTraversal: AppliedTraversal[A, (B, C)]) extends AnyVal {
  /** swap the elements of a Tuple */
  def swap: AppliedTraversal[A, (C, B)] = appliedTraversal.andThen(swapTuple[B, C])
}
