package proptics.applied.internal

import cats.Eq

import proptics.internal.Getter0

private[proptics] trait AppliedGetter0[S, A] extends Serializable {
  val value: S
  val optic: Getter0[S, A]

  /** find the first focus of a Getter that satisfies a predicate, if there is any */
  def find(f: A => Boolean): Option[A]

  /** check if the Getter does not contain a focus */
  final def isEmpty: Boolean = optic.isEmpty(value)

  /** check if the Getter contains a focus */
  final def nonEmpty: Boolean = optic.nonEmpty(value)

  /** test whether a predicate holds for the focus of a Getter */
  def exists(f: A => Boolean): Boolean

  /** test whether a predicate does not hold for the focus of a Getter */
  final def notExists(f: A => Boolean): Boolean = optic.notExists(f)(value)

  /** test whether the focus of a Getter contains a given value */
  final def contains(a: A)(s: S)(implicit ev: Eq[A]): Boolean = optic.contains(a)(value)

  /** test whether the focus of a Getter does not contain a given value */
  final def notContains(a: A)(implicit ev: Eq[A]): Boolean = optic.notContains(a)(value)
}
