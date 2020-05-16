package proptics.instances

import cats.{Monoid, Semigroup}
import spire.algebra.lattice.Heyting

private[proptics] trait BooleanInstances {
  implicit final val semigroupBoolAll: Semigroup[Boolean] = new Semigroup[Boolean] {
    override def combine(x: Boolean, y: Boolean): Boolean = heytingBool.and(x, y)
  }

  implicit final val semigroupBoolAny: Semigroup[Boolean] = new Semigroup[Boolean] {
    override def combine(x: Boolean, y: Boolean): Boolean = heytingBool.or(x, y)
  }

  implicit final val monoidBoolAll: Monoid[Boolean] = new Monoid[Boolean] {
    override def empty: Boolean = true

    override def combine(x: Boolean, y: Boolean): Boolean = semigroupBoolAll.combine(x, y)
  }

  implicit final val monoidBoolAny: Monoid[Boolean] = new Monoid[Boolean] {
    override def empty: Boolean = false

    override def combine(x: Boolean, y: Boolean): Boolean = semigroupBoolAny.combine(x, y)
  }

  implicit final def heytingBool: Heyting[Boolean] = new Heyting[Boolean] {
    override def and(a: Boolean, b: Boolean): Boolean = a && b

    override def or(a: Boolean, b: Boolean): Boolean = a || b

    override def imp(a: Boolean, b: Boolean): Boolean = !a || b

    override def complement(a: Boolean): Boolean = !a

    override def zero: Boolean = false

    override def one: Boolean = true
  }
}
