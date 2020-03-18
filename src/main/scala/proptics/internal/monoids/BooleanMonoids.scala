package proptics.internal.monoids

import cats.{Monoid, Semigroup}
import proptics.internal.heyting.HeytingInstances._

object BooleanMonoids {
  implicit final val semigroupBoolAll: Semigroup[Boolean] = new Semigroup[Boolean] {
    override def combine(x: Boolean, y: Boolean): Boolean = heytingBool.and(x, y)
  }

  implicit final val monoidBoolAll: Monoid[Boolean] = new Monoid[Boolean] {
    override def empty: Boolean = true

    override def combine(x: Boolean, y: Boolean): Boolean = semigroupBoolAll.combine(x, y)
  }
}
