package proptics

import cats.syntax.either._
import proptics.internal.Market

/**
 * * A [[Prism]] with fixed type [[Market]] [[cats.arrow.Profunctor]]
 *
 * @tparam S the source of an [[APrism]]
 * @tparam T the modified source of an [[APrism]]
 * @tparam A the target of an [[APrism]]
 * @tparam B the modified target of an [[APrism]]
 */
abstract class APrism[S, T, A, B] { self =>
  def apply(market: Market[A, B, A, B]): Market[A, B, S, T]

  def clonePrism[P[_, _]]: Prism[S, T, A, B] = self.withPrism(Prism[S, T, A, B])

  def withPrism[R](f: (B => T) => (S => Either[T, A]) => R): R = {
    val market = self(Market(identity, _.asRight[B]))

    f(market.to)(market.from)
  }
}

object APrism {
  private[proptics] def apply[S, T, A, B](f: Market[A, B, A, B] => Market[A, B, S, T]): APrism[S, T, A, B] = new APrism[S, T, A, B] {
    override def apply(pab: Market[A, B, A, B]): Market[A, B, S, T] = f(pab)
  }

  def apply[S, T, A, B](to: B => T)(from: S => Either[T, A]): APrism[S, T, A, B] =
    APrism((_: Market[A, B, A, B]) => Market(to, from))
}

object APrism_ {
  def apply[S, A](to: A => S)(from: S => Either[S, A]): APrism_[S, A] = APrism(to)(from)
}