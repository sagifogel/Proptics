package optics

import cats.arrow.Profunctor
import optics.internal.Exchange

/**
 * An Iso with fixed type [[Exchange]] [[cats.arrow.Profunctor]]
 *
 * @tparam S the source of a [[AnIso]]
 * @tparam T the modified source of a [[AnIso]]
 * @tparam A the target of a [[AnIso]]
 * @tparam B the modified target of a [[AnIso]]
 */
abstract class AnIso[S, T, A, B] extends Optic[Exchange[A, B, *, *], S, T, A, B] {
  def withIso[R](f: (S => A) => (B => T) => R): R

  def cloneIso[P[_, _]](implicit ev: Profunctor[P]): Iso[P, S, T, A, B] = withIso(Iso[P, S, T, A, B])
}

object AnIso {
  private[optics] def apply[S, T, A, B](f: Exchange[A, B, A, B] => Exchange[A, B, S, T]): AnIso[S, T, A, B] = new AnIso[S, T, A, B] { self =>
    override def withIso[R](f: (S => A) => (B => T) => R): R = {
      val exchange = self(Exchange(identity, identity))

      f(exchange.get)(exchange.inverseGet)
    }

    override def apply(pab: Exchange[A, B, A, B]): Exchange[A, B, S, T] = f(pab)
  }

  def apply[S, T, A, B](get: S => A)(inverseGet: B => T): AnIso[S, T, A, B] = {
    AnIso((e: Exchange[A, B, A, B]) => Exchange(e.get compose get, inverseGet compose e.inverseGet))
  }
}
