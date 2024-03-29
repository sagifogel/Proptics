package proptics.internal

import scala.Function.const

import cats.Eq
import cats.syntax.eq._

private[proptics] trait Getter0[S, A] extends Serializable {
  /** find the first focus of a Getter that satisfies a predicate, if there is any */
  def find(f: A => Boolean): S => Option[A]

  /** test whether a predicate holds for the focus of a Getter */
  def exists(f: A => Boolean): S => Boolean = s => find(f)(s).fold(false)(const(true))

  /** test whether a predicate does not hold for the focus of a Getter */
  final def notExists(f: A => Boolean): S => Boolean = s => !exists(f)(s)

  /** test whether the focus of a Getter contains a given value */
  final def contains(a: A)(s: S)(implicit ev: Eq[A]): Boolean = exists(_ === a)(s)

  /** test whether the focus of a Getter does not contain a given value */
  final def notContains(a: A)(s: S)(implicit ev: Eq[A]): Boolean = !contains(a)(s)
}
