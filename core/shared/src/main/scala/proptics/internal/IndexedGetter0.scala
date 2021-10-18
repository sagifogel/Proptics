package proptics.internal

import cats.Eq
import cats.syntax.eq._

trait IndexedGetter0[I, S, A] extends Serializable {
  protected def viewOption(s: S): Option[(A, I)]

  /** find if a focus of an IndexedGetter that satisfies a predicate */
  def find(f: ((A, I)) => Boolean): S => Option[(A, I)] = viewOption(_).filter(f)

  /** check if the IndexedGetter does not contain a focus */
  final def isEmpty(s: S): Boolean = viewOption(s).isEmpty

  /** check if the IndexedGetter contains a focus */
  final def nonEmpty(s: S): Boolean = !isEmpty(s)

  /** test whether a predicate holds for the focus of an IndexedGetter */
  def exists(f: ((A, I)) => Boolean): S => Boolean

  /** test whether a predicate does not hold for the focus of an IndexedGetter */
  final def notExists(f: ((A, I)) => Boolean): S => Boolean = s => !exists(f)(s)

  /** test whether a focus at specific index of an IndexedGetter contains a given value */
  final def contains(a: (A, I))(s: S)(implicit ev: Eq[(A, I)]): Boolean = exists(_ === a)(s)

  /** test whether a focus at specific index of an IndexedGetter does not contain a given value */
  final def notContains(a: (A, I))(s: S)(implicit ev: Eq[(A, I)]): Boolean = !contains(a)(s)
}
