package proptics.internal

import cats.Eq
import cats.syntax.eq._

private[proptics] trait Getter0[S, A] extends Serializable {
  protected def viewOption(s: S): Option[A]

  /** find the first focus of a Getter that satisfies a predicate, if there is any */
  def find(f: A => Boolean): S => Option[A]

  /** check if the Getter does not contain a focus */
  final def isEmpty(s: S): Boolean = viewOption(s).isEmpty

  /** check if the Getter contains a focus */
  final def nonEmpty(s: S): Boolean = !isEmpty(s)

  /** test whether a predicate holds for the focus of a Getter */
  def exists(f: A => Boolean): S => Boolean

  /** test whether a predicate does not hold for the focus of a Getter */
  final def notExists(f: A => Boolean): S => Boolean = s => !exists(f)(s)

  /** test whether the focus of a Getter contains a given value */
  final def contains(a: A)(s: S)(implicit ev: Eq[A]): Boolean = exists(_ === a)(s)

  /** test whether the focus of a Getter does not contain a given value */
  final def notContains(a: A)(s: S)(implicit ev: Eq[A]): Boolean = !contains(a)(s)
}
