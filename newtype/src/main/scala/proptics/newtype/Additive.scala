package proptics.newtype

import cats.kernel.Monoid
import spire.algebra.Semiring
import spire.syntax.semiring._

final case class Additive[A](runAdditive: A) extends AnyVal

abstract class AdditiveInstances {
  implicit final def monoidAdditive[A](implicit ev: Semiring[A]): Monoid[Additive[A]] = new Monoid[Additive[A]] {
    def empty: Additive[A] = Additive(ev.zero)

    def combine(x: Additive[A], y: Additive[A]): Additive[A] = Additive(x.runAdditive + y.runAdditive)
  }
}

object Additive extends AdditiveInstances