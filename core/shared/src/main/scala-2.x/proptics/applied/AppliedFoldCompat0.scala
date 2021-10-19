package proptics.applied

import spire.algebra.lattice.Heyting

import proptics.FoldCompat0
import proptics.applied.internal.{AppliedFold0, AppliedGetter0}

private[proptics] trait AppliedFoldCompat0[S, A] extends AppliedFold0[S, A] with AppliedGetter0[S, A] {
  val value: S
  val optic: FoldCompat0[S, A]

  /** test whether there is no focus or a predicate holds for the focus of a Fold, using a [[spire.algebra.lattice.Heyting]] algebra */
  final def forall[R: Heyting](f: A => R): R = optic.forall(value)(f)
}
