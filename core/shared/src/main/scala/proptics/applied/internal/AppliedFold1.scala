package proptics.applied.internal

import scala.reflect.ClassTag

import cats.Monoid
import cats.data.State
import cats.kernel.Order
import spire.algebra.lattice.Heyting
import spire.algebra.{AdditiveMonoid, MultiplicativeMonoid}

import proptics.internal.Fold1

private[proptics] trait AppliedFold1[S, A] extends AppliedFold0[S, A] {
  val value: S
  val optic: Fold1[S, A]

  /** synonym for [[fold]] */
  def view(implicit ev: Monoid[A]): A = optic.view(value)

  /** map each focus of a Fold to a [[cats.Monoid]], and combine the results */
  def foldMap[R: Monoid](f: A => R): R = optic.foldMap(value)(f)

  /** fold the foci of a Fold using a [[cats.Monoid]] */
  final def fold(implicit ev: Monoid[A]): A = optic.fold(value)

  /** fold the foci of a Fold using a binary operator, going right to left */
  final def foldRight[R](r: R)(f: (A, R) => R): R = optic.foldRight(value)(r)(f)

  /** fold the foci of a Fold using a binary operator, going left to right */
  final def foldLeft[R](r: R)(f: (R, A) => R): R = optic.foldLeft(value)(r)(f)

  /** collect all the foci of a Fold into aList */
  final def viewAll: List[A] = optic.viewAll(value)

  /** the number of foci of a Fold */
  final def length: Int = optic.length(value)

  /** find the first focus of a Fold, if there is any. Synonym for preview */
  final def first: Option[A] = optic.first(value)

  /** find the last focus of a Fold, if there is any */
  final def last: Option[A] = optic.last(value)

  /** the minimum of all foci of a Fold, if there is any */
  final def minimum(implicit ev: Order[A]): Option[A] = optic.minimum(value)

  /** the maximum of all foci of a Fold, if there is any */
  final def maximum(implicit ev: Order[A]): Option[A] = optic.maximum(value)

  /** collect all the foci of a Fold into an Array */
  final def toArray(implicit ev: ClassTag[A]): Array[A] = optic.toArray(value)

  /** synonym for [[viewAll]] */
  final def toList: List[A] = optic.toList(value)

  /** intercalate/insert an element between the existing elements while folding */
  final def intercalate(a: A)(implicit ev0: Monoid[A], ev1: S <:< Iterable[A]): A =
    optic.intercalate(value, a)

  /** displays all foci of a Fold in a string */
  final def mkString()(implicit ev: S <:< Iterable[A]): String = optic.mkString(value)

  /** displays all foci of a Fold in a string using a separator */
  final def mkString(sep: String)(implicit ev: S <:< Iterable[A]): String = optic.mkString(value, sep)

  /** displays all foci of a Fold in a string using a start, end and a separator */
  final def mkString(start: String, sep: String, end: String)(implicit ev: S <:< Iterable[A]): String =
    optic.mkString(value, start, sep, end)

  /** collect all the foci of a Fold in the state of a monad */
  final def use(implicit ev: State[S, A]): State[S, List[A]] = optic.use

  /** test whether there is no focus or a predicate holds for the focus of a Fold, using a [[spire.algebra.lattice.Heyting]] algebra */
  final def forallH[R: Heyting](f: A => R): R = optic.forall(value)(f)

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
