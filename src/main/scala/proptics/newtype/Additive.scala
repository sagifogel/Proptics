package proptics.newtype

import cats.syntax.monoid._
import cats.kernel.Monoid

final case class Additive[A](runAdditive: A) extends AnyVal

abstract class AdditiveInstances {
  implicit final def monoidAdditive[A](implicit ev: Monoid[A]): Monoid[Additive[A]] = new Monoid[Additive[A]] {
    def empty: Additive[A] = Additive(ev.empty)

    def combine(x: Additive[A], y: Additive[A]): Additive[A] = Additive(x.runAdditive |+| y.runAdditive)
  }
}

object Additive extends AdditiveInstances