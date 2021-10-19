package proptics.applied

import spire.algebra.lattice.Heyting
import spire.algebra.{AdditiveMonoid, MultiplicativeMonoid}

import proptics.FoldCompat
import proptics.applied.internal.AppliedFold1

private[proptics] trait AppliedFoldCompat[S, A] extends AppliedFoldCompat0[S, A] with AppliedFold1[S, A] {
  val value: S
  val optic: FoldCompat[S, A]

  /** the sum of all foci of a Fold */
  final def sum(implicit ev: AdditiveMonoid[A]): A = optic.sum(value)

  /** the product of all foci of a Fold */
  final def product(implicit ev: MultiplicativeMonoid[A]): A = optic.product(value)

  /** return the result of a conjunction of all foci of a Fold, using a [[spire.algebra.lattice.Heyting]] algebra */
  final def and(implicit ev: Heyting[A]): A = optic.and(value)

  /** return the result of a disjunction of all foci of a Fold, using a [[spire.algebra.lattice.Heyting]] algebra */
  final def or(implicit ev: Heyting[A]): A = optic.or(value)

  /** test whether a predicate holds for any focus of a Fold, using a [[spire.algebra.lattice.Heyting]] algebra */
  final def any[R: Heyting](f: A => R): R = optic.any(value)(f)
}
