package proptics

import cats.arrow.Profunctor
import proptics.internal.Exchange

/**
 * An Iso with fixed type [[Exchange]] [[cats.arrow.Profunctor]]
 *
 * @tparam S the source of a [[AnIso]]
 * @tparam T the modified source of a [[AnIso]]
 * @tparam A the target of a [[AnIso]]
 * @tparam B the modified target of a [[AnIso]]
 */
abstract class AnIso[S, T, A, B] { self =>
  def apply[P[_, _]](exchange: Exchange[A, B, A, B])(implicit ev: Profunctor[P]): Exchange[A, B, S, T]

  def cloneIso[P[_, _]](implicit ev: Profunctor[P]): Iso[S, T, A, B] = self.withIso(Iso[S, T, A, B])

  def withIso[P[_, _], R](f: (S => A) => (B => T) => R)(implicit ev: Profunctor[P]): R = {
    val exchange = self.apply(Exchange(identity, identity))

    f(exchange.get)(exchange.inverseGet)
  }
}

object AnIso {
  private[proptics] def apply[S, T, A, B](f: Exchange[A, B, A, B] => Exchange[A, B, S, T]): AnIso[S, T, A, B] = new AnIso[S, T, A, B] { self =>
    override def apply[P[_, _]](exchange: Exchange[A, B, A, B])(implicit ev: Profunctor[P]): Exchange[A, B, S, T] = f(exchange)
  }

  def apply[S, T, A, B](get: S => A)(inverseGet: B => T): AnIso[S, T, A, B] = {
    AnIso((ex: Exchange[A, B, A, B]) => Exchange(ex.get compose get, inverseGet compose ex.inverseGet))
  }
}
