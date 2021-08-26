package proptics

import cats.syntax.eq._
import cats.{Eq, Id, Monoid}
import spire.algebra.lattice.Heyting
import spire.algebra.{AdditiveMonoid, MultiplicativeMonoid}
import spire.std.boolean._

import proptics.data.{Additive, Conj, Disj, Multiplicative}
import proptics.syntax.tuple._

trait IndexedFoldCompat[S, I, A] extends Serializable {
  /** map each focus of a [[Fold_]] to a [[cats.Monoid]], and combine the results */
  def foldMap[R: Monoid](s: S)(f: ((A, I)) => R): R

  /** test whether a predicate hold for any focus and index of an [[IndexedFold_]] */
  final def exists(f: ((A, I)) => Boolean): S => Boolean = any[Disj, Boolean](_)(f)

  /** test whether a predicate does not hold for any focus and index of an [[IndexedFold_]] */
  final def notExists(f: ((A, I)) => Boolean): S => Boolean = !exists(f)(_)

  /** test whether a focus at specific index of an [[IndexedFold_]] contains a given value */
  final def contains(a: (A, I))(s: S)(implicit ev: Eq[(A, I)]): Boolean = exists(_ === a)(s)

  /** test whether a focus at specific index of an [[IndexedFold_]] does not contain a given value */
  final def notContains(a: (A, I))(s: S)(implicit ev: Eq[(A, I)]): Boolean = !contains(a)(s)
  /** the sum of all foci of an [[IndexedFold_]] */
  final def sum(s: S)(implicit ev: AdditiveMonoid[A]): A = foldMap(s)(Additive[A] _ compose Tuple2._1).runAdditive

  /** the product of all foci of an [[IndexedFold_]] */
  final def product(s: S)(implicit ev: MultiplicativeMonoid[A]): A = foldMap(s)(Multiplicative[A] _ compose Tuple2._1).runMultiplicative

  /** test whether there is no focus or a predicate holds for all foci and indices of an [[IndexedFold_]] */
  final def forall(f: ((A, I)) => Boolean): S => Boolean = s => forall(s)(f)

  /** test whether there is no focus or a predicate holds for all foci and indices of an [[IndexedFold_]], using a [[spire.algebra.lattice.Heyting]] algebra */
  final def forall[R: Heyting](s: S)(f: ((A, I)) => R): R = foldMap(s)(Conj[R] _ compose f).runConj

  /** return the result of a conjunction of all foci of an [[IndexedFold_]], using a [[spire.algebra.lattice.Heyting]] algebra */
  final def and(s: S)(implicit ev: Heyting[A]): A = forall(s)(_._1)

  /** return the result of a disjunction of all foci of an [[IndexedFold_]], using a [[spire.algebra.lattice.Heyting]] algebra */
  final def or(s: S)(implicit ev: Heyting[A]): A = any[Id, A](s)(_._1)

  /** test whether a predicate holds for any focus and index of an [[IndexedFold_]], using a [[spire.algebra.lattice.Heyting]] algebra */
  final def any[F[_], R: Heyting](s: S)(f: ((A, I)) => R): R = foldMap(s)(Disj[R] _ compose f).runDisj
}
