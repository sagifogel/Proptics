package proptics.newtype

import spire.syntax.semiring._
import cats.kernel.Monoid
import spire.algebra.Semiring

final case class Multiplicative[A](runMultiplicative: A) extends AnyVal

abstract class MultiplicativeInstances {
  implicit final def monoidMultiplicative[A](implicit ev: Semiring[A]): Monoid[Multiplicative[A]] = new Monoid[Multiplicative[A]] {
    def empty: Multiplicative[A] = Multiplicative(ev.zero)

    def combine(x: Multiplicative[A], y: Multiplicative[A]): Multiplicative[A] =
      Multiplicative(x.runMultiplicative * y.runMultiplicative)
  }
}

object Multiplicative extends MultiplicativeInstances