package proptics.internal

import cats.Monoid

import proptics.data.Disj

trait FoldInstances extends Serializable {
  implicit def monoidBooleanDisj: Monoid[Disj[Boolean]] = new Monoid[Disj[Boolean]] {
    override def empty: Disj[Boolean] = Disj(false)

    override def combine(x: Disj[Boolean], y: Disj[Boolean]): Disj[Boolean] = Disj(x.runDisj || y.runDisj)
  }
}
