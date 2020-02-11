package proptics

import proptics.internal.Market
import proptics.syntax.PrismSyntax._

/**
 * * A [[Prism]] with fixed type [[Market]] [[cats.arrow.Profunctor]]
 *
 * @tparam S the source of an [[APrism]]
 * @tparam T the modified source of an [[APrism]]
 * @tparam A the target of an [[APrism]]
 * @tparam B the modified target of an [[APrism]]
 */
abstract class APrism[S, T, A, B] extends Optic[Market[A, B, *, *], S, T, A, B] { self =>
  def clonePrism[P[_, _]]: Prism[S, T, A, B] = self.withPrism(Prism[S, T, A, B])
}

object APrism {
  private[proptics] def apply[S, T, A, B](f: Market[A, B, A, B] => Market[A, B, S, T]): APrism[S, T, A, B] = new APrism[S, T, A, B] {
    override def apply(pab: Market[A, B, A, B]): Market[A, B, S, T] = f(pab)
  }

  def apply[S, T, A, B](to: B => T)(from: S => Either[T, A]): APrism[S, T, A, B] =
    APrism((_: Market[A, B, A, B]) => Market(to, from))
}