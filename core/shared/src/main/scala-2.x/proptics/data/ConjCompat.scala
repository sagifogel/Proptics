package proptics.data

import cats.{Monoid, Semigroup}
import spire.algebra.Semiring
import spire.algebra.lattice.Heyting
import spire.syntax.semiring._

trait ConjCompat {
  implicit final def semigroupConj[A](implicit ev: Heyting[A]): Semigroup[Conj[A]] = new Semigroup[Conj[A]] {
    override def combine(x: Conj[A], y: Conj[A]): Conj[A] = Conj(ev.and(x.runConj, y.runConj))
  }

  implicit final def monoidConj[A](implicit ev: Heyting[A]): Monoid[Conj[A]] = new Monoid[Conj[A]] {
    override def empty: Conj[A] = Conj(ev.one)

    override def combine(x: Conj[A], y: Conj[A]): Conj[A] = semigroupConj.combine(x, y)
  }

  implicit final def semiringConj[A](implicit ev: Semiring[A]): Semiring[Conj[A]] = new Semiring[Conj[A]] {
    override def zero: Conj[A] = Conj(ev.zero)

    override def times(x: Conj[A], y: Conj[A]): Conj[A] = Conj(x.runConj * y.runConj)

    override def plus(x: Conj[A], y: Conj[A]): Conj[A] = Conj(x.runConj + y.runConj)
  }
}
