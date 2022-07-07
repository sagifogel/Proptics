package proptics.internal

import scala.Function.const
import scala.reflect.ClassTag

import cats.data.State
import cats.syntax.option._
import cats.{Id, Monoid, Order}
import spire.algebra.lattice.Heyting
import spire.algebra.{AdditiveMonoid, MultiplicativeMonoid}
import spire.std.boolean._

import proptics.data._
import proptics.syntax.function._
import proptics.syntax.tuple._

private[proptics] trait IndexedFold0[I, S, A] extends IndexedGetter0[I, S, A] with FoldInstances {
  /** check if the IndexedFold does not contain a focus */
  final def isEmpty(s: S): Boolean = preview(s).isEmpty

  /** check if the IndexedFold contains a focus */
  final def nonEmpty(s: S): Boolean = !isEmpty(s)

  /** map each focus of an IndexedFold to a [[cats.Monoid]], and combine the results */
  def foldMap[R: Monoid](s: S)(f: ((A, I)) => R): R

  /** fold the foci of a IndexedFold using a [[cats.Monoid]] */
  final def fold(s: S)(implicit ev: Monoid[A]): A = foldMap(s)(_._1)

  /** fold the foci and indices of an IndexedFold using a binary operator, going right to left */
  final def foldRight[R](s: S)(r: R)(f: ((A, I), R) => R): R = foldMap(s)(Endo[* => *, R] _ compose f.curried).runEndo(r)

  /** fold the foci and indices of an IndexedFold using a binary operator, going left to right */
  final def foldLeft[R](s: S)(r: R)(f: (R, (A, I)) => R): R =
    foldMap(s)(Dual[Endo[* => *, R]] _ compose Endo[* => *, R] compose f.curried.flip).runDual.runEndo(r)

  /** collect all the foci of an IndexedFold into a List */
  final def viewAll(s: S): List[(A, I)] = foldMap(s)(List(_))

  /** view the first focus of a Fold, if there is any */
  final def preview(s: S): Option[(A, I)] = foldMap(s)(a => First(a.some)).runFirst

  final override def exists(f: ((A, I)) => Boolean): S => Boolean = foldMap(_)(Disj[Boolean] _ compose f).runDisj

  /** the number of foci of an IndexedFold */
  final def length(s: S): Int = foldMap(s)(const(1))

  /** find the first focus of an IndexedFold that satisfies a predicate, if there is any */
  final def find(f: ((A, I)) => Boolean): S => Option[(A, I)] = s => foldRight[Option[(A, I)]](s)(None)((ai, op) => op.fold(if (f(ai)) ai.some else None)(Some[(A, I)]))

  /** synonym for [[preview]] */
  final def first(s: S): Option[(A, I)] = preview(s)

  /** find the last focus and index of an IndexedFold that satisfies a predicate, if there is any */
  final def last(s: S): Option[(A, I)] = foldMap(s)(ai => Last(ai.some)).runLast

  /** the minimum of all foci of an IndexedFold, if there is any */
  final def minimum(s: S)(implicit ev: Order[A]): Option[A] = minMax(s)(ev.min)

  /** the maximum of all foci of an IndexedFold, if there is any */
  final def maximum(s: S)(implicit ev: Order[A]): Option[A] = minMax(s)(ev.max)

  /** collect all the foci of an IndexedFold into an Array */
  final def toArray(s: S)(implicit ev0: ClassTag[A]): Array[A] = toList(s).toArray

  /** collect all the foci of an IndexedFold into aList */
  final def toList(s: S): List[A] = foldMap(s) { case (a, _) => List(a) }

  /** view the focus and the index of an IndexedFold in the state of a monad */
  final def use(implicit ev: State[S, A]): State[S, List[(A, I)]] = ev.inspect(viewAll)

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

  private[proptics] def minMax(s: S)(f: (A, A) => A): Option[A] =
    foldRight[Option[A]](s)(None)((pair, op) => f(pair._1, op.getOrElse(pair._1)).some)
}
