package optics

import algebra.lattice.Heyting
import cats.kernel.Semigroup
import spire.algebra.{Field, Ring, Semiring}

import scala.Function.const

/**
 * A [[Setter]] is an [[Optic]] with a fixed type of a [[Function1]] as the type constructor
 *
 * @tparam S the source of a [[Setter]]
 * @tparam T the modified source of a [[Setter]]
 * @tparam A the target of a [[Setter]]
 * @tparam B the modified target of a [[Setter]]
 */
abstract class Setter[S, T, A, B] extends Optic[* => *, S, T, A, B] { self =>
  def over(f: A => B, s: S): T = self(f)(s)

  def set(b: B, s: S): T = over(const(b), s)

}

object Setter {
  def addOver[S, T, A](setter: Setter[S, T, A, A])(a: A, s: S)(implicit ev: Semiring[A]): T =
    setter.over(ev.additive.combine(_, a), s)

  def mulOver[S, T, A](setter: Setter[S, T, A, A])(a: A, s: S)(implicit ev: Semiring[A]): T =
    setter.over(ev.multiplicative.combine(_, a), s)

  def subOver[S, T, A](setter: Setter[S, T, A, A])(a: A, s: S)(implicit ev: Ring[A]): T =
    setter.over(ev.minus(_, a), s)

  def divOver[S, T, A](setter: Setter[S, T, A, A])(a: A, s: S)(implicit ev: Field[A]): T =
    setter.over(ev.div(_, a), s)

  def disjOver[S, T, A](setter: Setter[S, T, A, A])(a: A, s: S)(implicit ev: Heyting[A]): T =
    setter.over(ev.or(_, a), s)

  def conjOver[S, T, A](setter: Setter[S, T, A, A])(a: A, s: S)(implicit ev: Heyting[A]): T =
    setter.over(ev.and(_, a), s)

  def appendOver[S, T, A](setter: Setter[S, T, A, A])(a: A, s: S)(implicit ev: Semigroup[A]): T =
    setter.over(ev.combine(_, a), s)
}
