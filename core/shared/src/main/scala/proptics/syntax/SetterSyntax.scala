package proptics.syntax

import algebra.lattice.Heyting
import cats.kernel.Semigroup
import spire.algebra.{Field, Ring, Semiring}

import proptics.Setter_

trait SetterSyntax {
  implicit def setterSTAAOps[S, T, A](setter: Setter_[S, T, A, A]): SetterSTAAOps[S, T, A] = SetterSTAAOps(setter)

  implicit def setterSTAOptionB[S, T, A, B](setter: Setter_[S, T, A, Option[B]]): SetterSTAOptionB[S, T, A, B] = SetterSTAOptionB(setter)
}

final case class SetterSTAAOps[S, T, A](private val setter: Setter_[S, T, A, A]) extends AnyVal {
  def addOver(a: A)(implicit ev0: Semiring[A]): S => T = setter.over(ev0.additive.combine(_, a))

  def mulOver(a: A)(implicit ev0: Semiring[A]): S => T = setter.over(ev0.multiplicative.combine(_, a))

  def subOver(a: A)(implicit ev0: Ring[A]): S => T = setter.over(ev0.minus(_, a))

  def divOver(a: A)(implicit ev0: Field[A]): S => T = setter.over(ev0.div(_, a))

  def disjOver(a: A)(implicit ev0: Heyting[A]): S => T = setter.over(ev0.or(_, a))

  def conjOver(a: A)(implicit ev0: Heyting[A]): S => T = setter.over(ev0.and(_, a))

  def appendOver(a: A)(implicit ev0: Semigroup[A]): S => T = setter.over(ev0.combine(_, a))
}

final case class SetterSTAOptionB[S, T, A, B](private val setter: Setter_[S, T, A, Option[B]]) extends AnyVal {
  def setJust(b: B): S => T = setter.set(Some(b))
}
