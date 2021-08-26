package proptics

import cats.syntax.eq._
import cats.{Eq, Monoid}
import spire.algebra.lattice.Heyting
import spire.std.boolean._

import proptics.data.{Conj, Disj}

trait FoldCompat0[S, A] extends Serializable {
  protected[proptics] def foldMap[R: Monoid](s: S)(f: A => R): R

  /** test whether there is no focus or a predicate holds for the focus of a [[Fold_]] */
  final def forall(f: A => Boolean): S => Boolean = forall(_)(f)

  /** test whether there is no focus or a predicate holds for the focus of a [[Fold_]], using a [[spire.algebra.lattice.Heyting]] algebra */
  final def forall[R: Heyting](s: S)(f: A => R): R = foldMap(s)(Conj[R] _ compose f).runConj

  /** test whether a predicate holds for the focus of a [[Fold_]] */
  final def exists(f: A => Boolean): S => Boolean = foldMap(_)(Disj[Boolean] _ compose f).runDisj

  /** test whether a predicate does not hold for the focus of a [[Fold_]] */
  final def notExists(f: A => Boolean): S => Boolean = s => !exists(f)(s)

  /** test whether the focus of a [[Fold_]] contains a given value */
  final def contains(a: A)(s: S)(implicit ev: Eq[A]): Boolean = exists(_ === a)(s)

  /** test whether the focus of a [[Fold_]] does not contain a given value */
  final def notContains(a: A)(s: S)(implicit ev: Eq[A]): Boolean = !contains(a)(s)
}
