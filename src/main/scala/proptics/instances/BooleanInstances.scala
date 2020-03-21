package proptics.instances

import spire.algebra.lattice.Heyting

object HeytingInstances {
  implicit final def heytingBool: Heyting[Boolean] = new Heyting[Boolean] {
    override def and(a: Boolean, b: Boolean): Boolean = a && b

    override def or(a: Boolean, b: Boolean): Boolean = a || b

    override def imp(a: Boolean, b: Boolean): Boolean = !a || b

    override def complement(a: Boolean): Boolean = !a

    override def zero: Boolean = false

    override def one: Boolean = true
  }
}
