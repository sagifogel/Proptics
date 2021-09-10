package proptics.data

import cats.{Monoid, Semigroup}
import spire.algebra.MultiplicativeMonoid
import spire.syntax.semiring._

trait MultiplicativeCompat {
  implicit final def semigroupMultiplicative[A: MultiplicativeMonoid]: Semigroup[Multiplicative[A]] = new Semigroup[Multiplicative[A]] {
    def combine(x: Multiplicative[A], y: Multiplicative[A]): Multiplicative[A] =
      Multiplicative(x.runMultiplicative * y.runMultiplicative)
  }

  implicit final def monoidMultiplicative[A](implicit ev: MultiplicativeMonoid[A]): Monoid[Multiplicative[A]] = new Monoid[Multiplicative[A]] {
    def empty: Multiplicative[A] = Multiplicative(ev.one)

    def combine(x: Multiplicative[A], y: Multiplicative[A]): Multiplicative[A] = semigroupMultiplicative.combine(x, y)
  }
}
