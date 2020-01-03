package optics

import optics.syntax.PrismSyntax._
import optics.internal.Market
import optics.profunctor.Choice


/**
 * * A [[Prism]] with fixed type [[Market]] [[cats.arrow.Profunctor]]
 *
 * @tparam S the source of an [[APrism]]
 * @tparam T the modified source of an [[APrism]]
 * @tparam A the target of an [[APrism]]
 * @tparam B the modified target of an [[APrism]]
 */
abstract class APrism[S, T, A, B] extends Optic[Market[A, B, *, *], S, T, A, B] { self =>
  def clonePrism[P[_, _]](implicit ev: Choice[P]): Prism[P, S, T, A, B] = self.withPrism(Prism[P, S, T, A, B])
}

object APrism {
  private[optics] def apply[S, T, A, B](f: Market[A, B, A, B] => Market[A, B, S, T]): APrism[S, T, A, B] = new APrism[S, T, A, B] {
    override def apply(pab: Market[A, B, A, B]): Market[A, B, S, T] = f(pab)
  }

  def apply[S, T, A, B](to: B => T)(from: S => Either[T, A]): APrism[S, T, A, B] =
    APrism((_: Market[A, B, A, B]) => Market(to, from))
}