package proptics.data

import cats.{Monoid, Semigroup}
import spire.algebra.AdditiveMonoid
import spire.syntax.semiring._

trait AdditiveCompat {
  implicit final def semigroupAdditive[A: AdditiveMonoid]: Semigroup[Additive[A]] = new Semigroup[Additive[A]] {
    override def combine(x: Additive[A], y: Additive[A]): Additive[A] = Additive(x.runAdditive + y.runAdditive)
  }

  implicit final def monoidAdditive[A](implicit ev: AdditiveMonoid[A]): Monoid[Additive[A]] = new Monoid[Additive[A]] {
    def empty: Additive[A] = Additive(ev.zero)

    def combine(x: Additive[A], y: Additive[A]): Additive[A] = Additive(x.runAdditive + y.runAdditive)
  }
}
