package proptics.newtype

import cats.Semigroup
import cats.kernel.Monoid

final case class Last[A](runLast: Option[A]) extends AnyVal

abstract class LastInstances {
  implicit final def semigroupLast[A]: Semigroup[Last[A]] = new Semigroup[Last[A]] {
    def combine(x: Last[A], y: Last[A]): Last[A] = Last(y.runLast orElse x.runLast)
  }

  implicit final def monoidLast[A]: Monoid[Last[A]] = new Monoid[Last[A]] {
    def empty: Last[A] = Last(None)

    def combine(x: Last[A], y: Last[A]): Last[A] = semigroupLast.combine(x, y)
  }
}

object Last extends LastInstances