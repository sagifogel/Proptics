package proptics.newtype

import algebra.lattice.Heyting
import cats.Semigroup
import cats.kernel.Monoid

final case class Conj[A](runConj: A) extends AnyVal

abstract class ConjInstances {
  implicit final def semigroupConj[A](implicit ev: Heyting[A]): Semigroup[Conj[A]] = new Semigroup[Conj[A]] {
    override def combine(x: Conj[A], y: Conj[A]): Conj[A] = Conj(ev.and(x.runConj, y.runConj))
  }

  implicit final def monoidConj[A](implicit ev: Heyting[A]): Monoid[Conj[A]] = new Monoid[Conj[A]] {
    override def empty: Conj[A] = Conj(ev.one)

    override def combine(x: Conj[A], y: Conj[A]): Conj[A] = semigroupConj.combine(x, y)
  }
}

object Conj extends ConjInstances
