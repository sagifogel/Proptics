package proptics.syntax

import algebra.lattice.Heyting
import cats.kernel.Semigroup
import spire.algebra.{Field, Ring, Semiring}

import proptics.Setter_

trait SetterSyntaxCompat {
  implicit def setterSTAAOps[S, T, A](setter: Setter_[S, T, A, A]): SetterSTAAOps[S, T, A] = SetterSTAAOps(setter)
}

final case class SetterSTAAOps[S, T, A](private val setter: Setter_[S, T, A, A]) extends AnyVal {
  /** modify the focus type of a [[Setter_]] using a Semiring additive */
  def addOver(s: S)(a: A)(implicit ev0: Semiring[A]): T = setter.over(ev0.additive.combine(_, a))(s)

  /** modify the focus type of a [[Setter_]] using a Semiring multiplicative */
  def mulOver(s: S)(a: A)(implicit ev0: Semiring[A]): T = setter.over(ev0.multiplicative.combine(_, a))(s)

  /** modify the focus type of a [[Setter_]] using a Ring minus */
  def subOver(s: S)(a: A)(implicit ev0: Ring[A]): T = setter.over(ev0.minus(_, a))(s: S)

  /** modify the focus type of a [[Setter_]] using a Field div */
  def divOver(s: S)(a: A)(implicit ev0: Field[A]): T = setter.over(ev0.div(_, a))(s)

  /** modify the focus type of a [[Setter_]] using a Heyting or */
  def disjOver(s: S)(a: A)(implicit ev0: Heyting[A]): T = setter.over(ev0.or(_, a))(s)

  /** modify the focus type of a [[Setter_]] using a Heyting and */
  def conjOver(s: S)(a: A)(implicit ev0: Heyting[A]): T = setter.over(ev0.and(_, a))(s)

  /** modify the focus type of a [[Setter_]] using a Semigroup combine */
  def appendOver(s: S)(a: A)(implicit ev0: Semigroup[A]): T = setter.over(ev0.combine(_, a))(s)
}
