package proptics.newtype

import cats.kernel.Monoid
import cats.syntax.monoid._

final case class Dual[A](runDual: A) extends AnyVal

abstract class DualInstances {
  implicit final def monoidDual[A](implicit ev: Monoid[A]): Monoid[Dual[A]] = new Monoid[Dual[A]] {
    def empty: Dual[A] = Dual(ev.empty)

    def combine(x: Dual[A], y: Dual[A]): Dual[A] = Dual(x.runDual |+| y.runDual)
  }
}

object Dual extends DualInstances