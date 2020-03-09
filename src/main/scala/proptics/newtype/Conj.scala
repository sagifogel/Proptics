package proptics.newtype

import algebra.lattice.Heyting
import cats.kernel.Monoid

final case class Conj[A](runConj: A) extends AnyVal

abstract class ConjInstances {
  implicit final def monoidConj[A](implicit ev: Heyting[A]): Monoid[Conj[A]] = new Monoid[Conj[A]] {
    override def empty: Conj[A] = Conj(ev.one)

    override def combine(x: Conj[A], y: Conj[A]): Conj[A] = Conj(ev.and(x.runConj, y.runConj))
  }
}

object Conj extends ConjInstances
