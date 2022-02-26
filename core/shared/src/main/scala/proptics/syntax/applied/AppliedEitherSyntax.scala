package proptics.syntax.applied

import proptics._
import proptics.std.either._

trait AppliedEitherSyntax {
  implicit final def appliedLensSwapEitherOps[A, B, C](appliedLens: AppliedLens[A, Either[B, C]]): AppliedLensSwapEitherOps[A, B, C] =
    AppliedLensSwapEitherOps(appliedLens)

  implicit final def appliedPrismSwapEitherOps[A, B, C](appliedPrism: AppliedPrism[A, Either[B, C]]): AppliedPrismSwapEitherOps[A, B, C] =
    AppliedPrismSwapEitherOps(appliedPrism)

  implicit final def appliedAffineTraversalSwapEitherOps[A, B, C](appliedAffineTraversal: AppliedAffineTraversal[A, Either[B, C]]): AppliedAffineTraversalSwapEitherOps[A, B, C] =
    AppliedAffineTraversalSwapEitherOps(appliedAffineTraversal)

  implicit final def appliedFoldSwapEitherOps[A, B, C](appliedFold: AppliedFold[A, Either[B, C]]): AppliedFoldSwapEitherOps[A, B, C] =
    AppliedFoldSwapEitherOps(appliedFold)

  implicit final def appliedTraversalSwapEitherOps[A, B, C](appliedLens: AppliedTraversal[A, Either[B, C]]): AppliedTraversalSwapEitherOps[A, B, C] =
    AppliedTraversalSwapEitherOps(appliedLens)
}

final case class AppliedLensSwapEitherOps[A, B, C](private val appliedLens: AppliedLens[A, Either[B, C]]) extends AnyVal {
  /** swap the elements of an Either */
  def swap: AppliedLens[A, Either[C, B]] = appliedLens.andThen(swapEither[B, C])
}

final case class AppliedPrismSwapEitherOps[A, B, C](private val appliedPrism: AppliedPrism[A, Either[B, C]]) extends AnyVal {
  /** swap the elements of an Either */
  def swap: AppliedPrism[A, Either[C, B]] = appliedPrism.andThen(swapEither[B, C])
}

final case class AppliedAffineTraversalSwapEitherOps[A, B, C](private val appliedAffineTraversal: AppliedAffineTraversal[A, Either[B, C]]) extends AnyVal {
  /** swap the elements of an Either */
  def swap: AppliedAffineTraversal[A, Either[C, B]] = appliedAffineTraversal.andThen(swapEither[B, C])
}

final case class AppliedFoldSwapEitherOps[A, B, C](private val appliedFold: AppliedFold[A, Either[B, C]]) extends AnyVal {
  /** swap the elements of an Either */
  def swap: AppliedFold[A, Either[C, B]] = appliedFold.andThen(swapEither[B, C])
}

final case class AppliedTraversalSwapEitherOps[A, B, C](private val appliedTraversal: AppliedTraversal[A, Either[B, C]]) extends AnyVal {
  /** swap the elements of an Either */
  def swap: AppliedTraversal[A, Either[C, B]] = appliedTraversal.andThen(swapEither[B, C])
}
