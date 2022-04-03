package proptics.syntax.applied

import proptics._
import proptics.std.either.{left => lft, right => rght, _}

trait AppliedEitherSyntax {
  implicit final def appliedLensEitherOps[A, B, C](appliedLens: AppliedLens[A, Either[B, C]]): AppliedLensEitherOps[A, B, C] =
    AppliedLensEitherOps(appliedLens)

  implicit final def appliedFoldEitherOps[A, B, C](appliedFold: AppliedFold[A, Either[B, C]]): AppliedFoldEitherOps[A, B, C] =
    AppliedFoldEitherOps(appliedFold)

  implicit final def appliedPrismEitherOps[A, B, C](appliedPrism: AppliedPrism[A, Either[B, C]]): AppliedPrismEitherOps[A, B, C] =
    AppliedPrismEitherOps(appliedPrism)

  implicit final def appliedAffineTraversalEitherOps[A, B, C](appliedAffineTraversal: AppliedAffineTraversal[A, Either[B, C]]): AppliedAffineTraversalEitherOps[A, B, C] =
    AppliedAffineTraversalEitherOps(appliedAffineTraversal)

  implicit final def appliedTraversalEitherOps[A, B, C](appliedLens: AppliedTraversal[A, Either[B, C]]): AppliedTraversalEitherOps[A, B, C] =
    AppliedTraversalEitherOps(appliedLens)
}

final case class AppliedLensEitherOps[A, B, C](private val appliedLens: AppliedLens[A, Either[B, C]]) extends AnyVal {
  /** swap the elements of an Either */
  def swap: AppliedLens[A, Either[C, B]] = appliedLens.andThen(swapEither[B, C])

  /** extract the right element of an either */
  def right: AppliedAffineTraversal[A, C] = appliedLens.andThen(rght[B, C])

  /** extract the left element of an either */
  def left: AppliedAffineTraversal[A, B] = appliedLens.andThen(lft[B, C])
}

final case class AppliedPrismEitherOps[A, B, C](private val appliedPrism: AppliedPrism[A, Either[B, C]]) extends AnyVal {
  /** swap the elements of an Either */
  def swap: AppliedPrism[A, Either[C, B]] = appliedPrism.andThen(swapEither[B, C])

  /** extract the right element of an either */
  def right: AppliedPrism[A, C] = appliedPrism.andThen(rght[B, C])

  /** extract the left element of an either */
  def left: AppliedPrism[A, B] = appliedPrism.andThen(lft[B, C])
}

final case class AppliedAffineTraversalEitherOps[A, B, C](private val appliedAffineTraversal: AppliedAffineTraversal[A, Either[B, C]]) extends AnyVal {
  /** swap the elements of an Either */
  def swap: AppliedAffineTraversal[A, Either[C, B]] = appliedAffineTraversal.andThen(swapEither[B, C])

  /** extract the right element of an either */
  def right: AppliedAffineTraversal[A, C] = appliedAffineTraversal.andThen(rght[B, C])

  /** extract the left element of an either */
  def left: AppliedAffineTraversal[A, B] = appliedAffineTraversal.andThen(lft[B, C])
}

final case class AppliedFoldEitherOps[A, B, C](private val appliedFold: AppliedFold[A, Either[B, C]]) extends AnyVal {
  /** swap the elements of an Either */
  def swap: AppliedFold[A, Either[C, B]] = appliedFold.andThen(swapEither[B, C])

  /** extract the right element of an either */
  def right: AppliedFold[A, C] = appliedFold.andThen(rght[B, C])

  /** extract the left element of an either */
  def left: AppliedFold[A, B] = appliedFold.andThen(lft[B, C])
}

final case class AppliedTraversalEitherOps[A, B, C](private val appliedTraversal: AppliedTraversal[A, Either[B, C]]) extends AnyVal {
  /** swap the elements of an Either */
  def swap: AppliedTraversal[A, Either[C, B]] = appliedTraversal.andThen(swapEither[B, C])

  /** extract the right element of an either */
  def right: AppliedTraversal[A, C] = appliedTraversal.andThen(rght[B, C])

  /** extract the left element of an either */
  def left: AppliedTraversal[A, B] = appliedTraversal.andThen(lft[B, C])
}
