package proptics.newtype

import cats.syntax.semigroup._
import cats.{Monoid, Semigroup}

final case class Dual[A](runDual: A) extends AnyVal

abstract class DualInstances {
  implicit final def semigroupDual[A](implicit ev: Semigroup[A]): Semigroup[Dual[A]] = new Semigroup[Dual[A]] {
    def combine(x: Dual[A], y: Dual[A]): Dual[A] = Dual(x.runDual |+| y.runDual)
  }

  implicit final def monoidDual[A](implicit ev: Monoid[A]): Monoid[Dual[A]] = new Monoid[Dual[A]] {
    def empty: Dual[A] = Dual(ev.empty)

    def combine(x: Dual[A], y: Dual[A]): Dual[A] = Dual(x.runDual |+| y.runDual)
  }
}

object Dual extends DualInstances