package proptics

import spire.algebra.lattice.Heyting

import proptics.data.Conj
import proptics.internal._

private[proptics] trait FoldCompat0[S, A] extends Fold0[S, A] with Getter0[S, A] {
  /** test whether there is no focus or a predicate holds for the focus of a Fold, using a [[spire.algebra.lattice.Heyting]] algebra */
  final def forall[R: Heyting](s: S)(f: A => R): R = foldMap(s)(Conj[R] _ compose f).runConj
}
