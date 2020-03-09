package proptics.newtype

import cats.kernel.Monoid

final case class First[A](runFirst: Option[A]) extends AnyVal

abstract class FirstInstances {
  implicit final def monoidFirst[A]: Monoid[First[A]] = new Monoid[First[A]] {
    def empty: First[A] = First(None)

    def combine(x: First[A], y: First[A]): First[A] = First(x.runFirst orElse y.runFirst)
  }
}

object First extends FirstInstances
