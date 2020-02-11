package proptics

import cats.arrow.Profunctor
import proptics.internal.Exchange
import proptics.syntax.IsoSyntax._

/**
 * An Iso with fixed type [[Exchange]] [[cats.arrow.Profunctor]]
 *
 * @tparam S the source of a [[AnIso]]
 * @tparam T the modified source of a [[AnIso]]
 * @tparam A the target of a [[AnIso]]
 * @tparam B the modified target of a [[AnIso]]
 */
abstract class AnIso[S, T, A, B] extends Optic[Exchange[A, B, *, *], S, T, A, B] { self =>
  def cloneIso[P[_, _]](implicit ev: Profunctor[P]): Iso[S, T, A, B] = self.withIso(Iso[S, T, A, B])
}

object AnIso {
  private[proptics] def apply[S, T, A, B](f: Exchange[A, B, A, B] => Exchange[A, B, S, T]): AnIso[S, T, A, B] = new AnIso[S, T, A, B] { self =>
    override def apply(pab: Exchange[A, B, A, B]): Exchange[A, B, S, T] = f(pab)
  }

  def apply[S, T, A, B](get: S => A)(inverseGet: B => T): AnIso[S, T, A, B] = {
    AnIso((ex: Exchange[A, B, A, B]) => Exchange(ex.get compose get, inverseGet compose ex.inverseGet))
  }
}
