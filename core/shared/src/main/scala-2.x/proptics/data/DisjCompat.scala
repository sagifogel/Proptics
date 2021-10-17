package proptics.data

import cats.{Monoid, Semigroup}
import spire.algebra.Semiring
import spire.algebra.lattice.Heyting
import spire.syntax.semiring._

trait DisjCompat {
  implicit final def semigroupHDisj[A](implicit ev: Heyting[A]): Semigroup[Disj[A]] = new Semigroup[Disj[A]] {
    override def combine(x: Disj[A], y: Disj[A]): Disj[A] = Disj(ev.or(x.runDisj, y.runDisj))
  }

  implicit final def monoidHDisj[A](implicit ev: Heyting[A]): Monoid[Disj[A]] = new Monoid[Disj[A]] {
    override def empty: Disj[A] = Disj(ev.zero)

    override def combine(x: Disj[A], y: Disj[A]): Disj[A] = semigroupHDisj.combine(x, y)
  }

  implicit final def semiringDisj[A](implicit ev: Semiring[A]): Semiring[Disj[A]] = new Semiring[Disj[A]] {
    override def zero: Disj[A] = Disj(ev.zero)

    override def times(x: Disj[A], y: Disj[A]): Disj[A] = Disj(x.runDisj * y.runDisj)

    override def plus(x: Disj[A], y: Disj[A]): Disj[A] = Disj(x.runDisj + y.runDisj)
  }
}
