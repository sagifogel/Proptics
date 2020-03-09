package proptics.newtype

import cats.kernel.Monoid

final case class Last[A](runLast: Option[A]) extends AnyVal

abstract class LastInstances {
  implicit final def monoidLast[A]: Monoid[Last[A]] = new Monoid[Last[A]] {
    def empty: Last[A] = Last(None)

    def combine(x: Last[A], y: Last[A]): Last[A] = Last(y.runLast orElse x.runLast)
  }
}

object Last extends LastInstances