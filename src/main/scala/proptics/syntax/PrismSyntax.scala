package proptics.syntax

import algebra.lattice.Heyting
import cats.syntax.either._
import proptics.Optic
import proptics.internal.Market

import scala.Function.const

object PrismSyntax {
  implicit class APrismOps[S, T, A, B](val aPrism: Optic[Market[A, B, *, *], S, T, A, B]) extends AnyVal {
    def withPrism[R](f: (B => T) => (S => Either[T, A]) => R): R = {
      val market = aPrism(Market(identity, _.asRight[B]))

      f(market.to)(market.from)
    }

    def matching(s: S): Either[T, A] = withPrism(const(_.apply(s)))

    def is[R](s: S)(implicit ev: Heyting[R]): R = matching(s).fold(const(ev.zero), const(ev.one))

    def imp[R](a: R, b: R)(implicit ev: Heyting[R]): R = ev.or(ev.complement(a), b)

    def isnt[R](s: S)(implicit ev: Heyting[R]): R = ev.imp(is(s), ev.zero)
  }
}
