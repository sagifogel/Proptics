package proptics

import cats.Id
import spire.algebra.lattice.Heyting
import spire.algebra.{AdditiveMonoid, MultiplicativeMonoid}
import spire.std.boolean._

import proptics.data.{Additive, Conj, Disj, Multiplicative}
import proptics.internal.{IndexedFold0, IndexedGetter0}
import proptics.syntax.tuple._

private[proptics] trait IndexedFoldCompat[I, S, A] extends IndexedFold0[I, S, A] with IndexedGetter0[I, S, A] {
  /** the sum of all foci of an IndexedFold */
  final def sum(s: S)(implicit ev: AdditiveMonoid[A]): A = foldMap(s)(Additive[A] _ compose Tuple2._1).runAdditive

  /** the product of all foci of an IndexedFold */
  final def product(s: S)(implicit ev: MultiplicativeMonoid[A]): A = foldMap(s)(Multiplicative[A] _ compose Tuple2._1).runMultiplicative

  /** test whether there is no focus or a predicate holds for all foci and indices of an IndexedFold */
  final def forall(f: ((A, I)) => Boolean): S => Boolean = s => forall(s)(f)

  /** test whether there is no focus or a predicate holds for all foci and indices of an IndexedFold, using a [[spire.algebra.lattice.Heyting]] algebra */
  final def forall[R: Heyting](s: S)(f: ((A, I)) => R): R = foldMap(s)(Conj[R] _ compose f).runConj

  /** return the result of a conjunction of all foci of an IndexedFold, using a [[spire.algebra.lattice.Heyting]] algebra */
  final def and(s: S)(implicit ev: Heyting[A]): A = forall(s)(_._1)

  /** return the result of a disjunction of all foci of an IndexedFold, using a [[spire.algebra.lattice.Heyting]] algebra */
  final def or(s: S)(implicit ev: Heyting[A]): A = any[Id, A](s)(_._1)

  /** test whether a predicate holds for any focus and index of an IndexedFold, using a [[spire.algebra.lattice.Heyting]] algebra */
  final def any[F[_], R: Heyting](s: S)(f: ((A, I)) => R): R = foldMap(s)(Disj[R] _ compose f).runDisj
}
