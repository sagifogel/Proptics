package proptics

import spire.algebra.lattice.Heyting
import spire.algebra.{AdditiveMonoid, MultiplicativeMonoid}

import proptics.data.{Additive, Disj, Multiplicative}
import proptics.internal.Fold1

trait FoldCompat[S, A] extends FoldCompat0[S, A] with Fold1[S, A] {
  /** the sum of all foci of a [[Fold_]] */
  final def sum(s: S)(implicit ev: AdditiveMonoid[A]): A = foldMap(s)(Additive.apply).runAdditive

  /** the product of all foci of a [[Fold_]] */
  final def product(s: S)(implicit ev: MultiplicativeMonoid[A]): A = foldMap(s)(Multiplicative.apply).runMultiplicative

  /** return the result of a conjunction of all foci of a [[Fold_]], using a [[spire.algebra.lattice.Heyting]] algebra */
  final def and(s: S)(implicit ev: Heyting[A]): A = forall(s)(identity)

  /** return the result of a disjunction of all foci of a [[Fold_]], using a [[spire.algebra.lattice.Heyting]] algebra */
  final def or(s: S)(implicit ev: Heyting[A]): A = any[A](s)(identity)

  /** test whether a predicate holds for any focus of a [[Fold_]], using a [[spire.algebra.lattice.Heyting]] algebra */
  final def any[R: Heyting](s: S)(f: A => R): R = foldMap(s)(Disj[R] _ compose f).runDisj
}
