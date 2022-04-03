package proptics.syntax.applied

import proptics._
import proptics.std.option.{none => non, some => sme}

trait AppliedOptionSyntax {
  implicit final def appliedLensOptionOps[A, B](appliedLens: AppliedLens[A, Option[B]]): AppliedLensOptionOps[A, B] =
    AppliedLensOptionOps(appliedLens)

  implicit final def appliedFoldOptionOps[A, B](appliedFold: AppliedFold[A, Option[B]]): AppliedFoldOptionOps[A, B] =
    AppliedFoldOptionOps(appliedFold)

  implicit final def appliedPrismOptionOps[A, B](appliedPrism: AppliedPrism[A, Option[B]]): AppliedPrismOptionOps[A, B] =
    AppliedPrismOptionOps(appliedPrism)

  implicit final def appliedAffineTraversalOptionOps[A, B](appliedAffineTraversal: AppliedAffineTraversal[A, Option[B]]): AppliedAffineTraversalOptionOps[A, B] =
    AppliedAffineTraversalOptionOps(appliedAffineTraversal)

  implicit final def appliedTraversalOptionOps[A, B](appliedLens: AppliedTraversal[A, Option[B]]): AppliedTraversalOptionOps[A, B] =
    AppliedTraversalOptionOps(appliedLens)
}

final case class AppliedLensOptionOps[A, B](private val appliedLens: AppliedLens[A, Option[B]]) extends AnyVal {
  /** extract B from an [[Option]] in case it is a [[Some]] */
  def some: AppliedAffineTraversal[A, B] = appliedLens.andThen(sme[B])

  /** extract [[Unit]] from an [[Option]] in case it is a [[None]] */
  def none: AppliedAffineTraversal[A, Unit] = appliedLens.andThen(non[B])
}

final case class AppliedFoldOptionOps[A, B](private val appliedFold: AppliedFold[A, Option[B]]) extends AnyVal {
  /** extract B from an [[Option]] in case it is a [[Some]] */
  def some: AppliedFold[A, B] = appliedFold.andThen(sme[B])

  /** extract [[Unit]] from an [[Option]] in case it is a [[None]] */
  def none: AppliedFold[A, Unit] = appliedFold.andThen(non[B])
}

final case class AppliedPrismOptionOps[A, B](private val appliedPrism: AppliedPrism[A, Option[B]]) extends AnyVal {
  /** extract B from an [[Option]] in case it is a [[Some]] */
  def some: AppliedPrism[A, B] = appliedPrism.andThen(sme[B])

  /** extract [[Unit]] from an [[Option]] in case it is a [[None]] */
  def none: AppliedPrism[A, Unit] = appliedPrism.andThen(non[B])
}

final case class AppliedAffineTraversalOptionOps[A, B](private val appliedAffineTraversal: AppliedAffineTraversal[A, Option[B]]) extends AnyVal {
  /** extract B from an [[Option]] in case it is a [[Some]] */
  def some: AppliedAffineTraversal[A, B] = appliedAffineTraversal.andThen(sme[B])

  /** extract [[Unit]] from an [[Option]] in case it is a [[None]] */
  def none: AppliedAffineTraversal[A, Unit] = appliedAffineTraversal.andThen(non[B])
}

final case class AppliedTraversalOptionOps[A, B](private val appliedTraversal: AppliedTraversal[A, Option[B]]) extends AnyVal {
  /** extract B from an [[Option]] in case it is a [[Some]] */
  def some: AppliedTraversal[A, B] = appliedTraversal.andThen(sme[B])

  /** extract [[Unit]] from an [[Option]] in case it is a [[None]] */
  def none: AppliedTraversal[A, Unit] = appliedTraversal.andThen(non[B])
}
