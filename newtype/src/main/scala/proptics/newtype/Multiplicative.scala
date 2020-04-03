package proptics.newtype

import cats.Semigroup
import cats.kernel.Monoid
import spire.algebra.Semiring
import spire.syntax.semiring._

final case class Multiplicative[A](runMultiplicative: A) extends AnyVal

abstract class MultiplicativeInstances {
  implicit final def semigroupMultiplicative[A](implicit ev: Semiring[A]): Semigroup[Multiplicative[A]] = new Semigroup[Multiplicative[A]] {
    def empty: Multiplicative[A] = Multiplicative(ev.zero)

    def combine(x: Multiplicative[A], y: Multiplicative[A]): Multiplicative[A] =
      Multiplicative(x.runMultiplicative * y.runMultiplicative)
  }

  implicit final def monoidMultiplicative[A](implicit ev: Semiring[A]): Monoid[Multiplicative[A]] = new Monoid[Multiplicative[A]] {
    def empty: Multiplicative[A] = Multiplicative(ev.zero)

    def combine(x: Multiplicative[A], y: Multiplicative[A]): Multiplicative[A] = semigroupMultiplicative.combine(x, y)
  }
}

object Multiplicative extends MultiplicativeInstances