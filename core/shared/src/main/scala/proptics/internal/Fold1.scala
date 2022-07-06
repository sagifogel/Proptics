package proptics.internal

import scala.Function.const
import scala.reflect.ClassTag

import cats.Monoid
import cats.data.State
import cats.kernel.Order
import cats.syntax.option._
import spire.algebra.lattice.Heyting
import spire.algebra.{AdditiveMonoid, MultiplicativeMonoid}

import proptics.data.{Additive, Disj, Dual, Endo, Last, Multiplicative}
import proptics.syntax.function._

private[proptics] trait Fold1[S, A] extends Fold0[S, A] {
  /** synonym for [[fold]] */
  def view(s: S)(implicit ev: Monoid[A]): A

  /** map each focus of a Fold to a [[cats.Monoid]], and combine the results */
  def foldMap[R: Monoid](s: S)(f: A => R): R

  /** fold the foci of a Fold using a [[cats.Monoid]] */
  final def fold(s: S)(implicit ev: Monoid[A]): A = foldMap(s)(identity)

  /** fold the foci of a Fold using a binary operator, going right to left */
  final def foldRight[R](s: S)(r: R)(f: (A, R) => R): R = foldMap(s)(Endo[* => *, R] _ compose f.curried).runEndo(r)

  /** fold the foci of a Fold using a binary operator, going left to right */
  final def foldLeft[R](s: S)(r: R)(f: (R, A) => R): R =
    foldMap(s)(Dual[Endo[* => *, R]] _ compose Endo[* => *, R] compose f.curried.flip).runDual.runEndo(r)

  /** test whether there is no focus or a predicate holds for the focus of a Fold */
  final override def forall(f: A => Boolean): S => Boolean = foldLeft(_)(true)((acc, a) => acc && f(a))

  /** collect all the foci of a Fold into aList */
  final def viewAll(s: S): List[A] = foldMap(s)(List(_))

  /** find the focus of an Fold that satisfies a predicate, if there is any */
  final override def find(f: A => Boolean): S => Option[A] =
    foldRight[Option[A]](_)(None)((a, b) => b.fold(if (f(a)) a.some else None)(Some[A]))

  /** the number of foci of a Fold */
  final def length(s: S): Int = foldMap(s)(const(1))

  /** find the first focus of a Fold, if there is any. Synonym for preview */
  final def first(s: S): Option[A] = preview(s)

  /** find the last focus of a Fold, if there is any */
  final def last(s: S): Option[A] = foldMap(s)(a => Last(a.some)).runLast

  /** the minimum of all foci of a Fold, if there is any */
  final def minimum(s: S)(implicit ev: Order[A]): Option[A] = minMax(s)(ev.min)

  /** the maximum of all foci of a Fold, if there is any */
  final def maximum(s: S)(implicit ev: Order[A]): Option[A] = minMax(s)(ev.max)

  /** collect all the foci of a Fold into an Array */
  final def toArray[AA >: A](s: S)(implicit ev: ClassTag[AA]): Array[AA] = toList(s).toArray

  /** synonym for [[viewAll]] */
  final def toList(s: S): List[A] = viewAll(s)

  /** collect all the foci of a Fold in the state of a monad */
  final def use(implicit ev: State[S, A]): State[S, List[A]] = ev.inspect(viewAll)

  /** intercalate/insert an element between the existing elements while folding */
  final def intercalate(s: S, a: A)(implicit ev0: Monoid[A], ev1: S <:< Iterable[A]): A =
    ev1(s).reduceOption(ev0.intercalate(a).combine _).getOrElse(ev0.empty)

  /** displays all foci of a Fold in a string */
  final def mkString(s: S)(implicit ev: S <:< Iterable[A]): String = ev(s).mkString

  /** displays all foci of a Fold in a string using a separator */
  final def mkString(s: S, sep: String)(implicit ev: S <:< Iterable[A]): String = ev(s).mkString(sep)

  /** displays all foci of a Fold in a string using a start, end and a separator */
  final def mkString(s: S, start: String, sep: String, end: String)(implicit ev: S <:< Iterable[A]): String =
    ev(s).mkString(start, sep, end)

  /** the sum of all foci of a Fold */
  final def sum(s: S)(implicit ev: AdditiveMonoid[A]): A = foldMap(s)(Additive.apply).runAdditive

  /** the product of all foci of a Fold */
  final def product(s: S)(implicit ev: MultiplicativeMonoid[A]): A = foldMap(s)(Multiplicative.apply).runMultiplicative

  /** return the result of a conjunction of all foci of a Fold, using a [[spire.algebra.lattice.Heyting]] algebra */
  final def and(s: S)(implicit ev: Heyting[A]): A = forall(s)(identity)

  /** return the result of a disjunction of all foci of a Fold, using a [[spire.algebra.lattice.Heyting]] algebra */
  final def or(s: S)(implicit ev: Heyting[A]): A = any[A](s)(identity)

  /** test whether a predicate holds for any focus of a Fold, using a [[spire.algebra.lattice.Heyting]] algebra */
  final def any[R: Heyting](s: S)(f: A => R): R = foldMap(s)(Disj[R] _ compose f).runDisj

  protected[proptics] def minMax(s: S)(f: (A, A) => A): Option[A] =
    foldLeft[Option[A]](s)(None)((op, a) => f(a, op.getOrElse(a)).some)
}
