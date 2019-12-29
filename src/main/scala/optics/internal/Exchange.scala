package optics.internal

import cats.Functor
import cats.arrow.Profunctor

/** The [[Exchange]] profunctor characterizes an [[optics.Iso]] */
final case class Exchange[A, B, S, T](get: S => A, inverseGet: B => T)

abstract class ExchangeInstances {
  implicit final def functorExchange[C, D, S]: Functor[Exchange[C, D, S, *]] = new Functor[Exchange[C, D, S, *]] {
    override def map[A, B](fa: Exchange[C, D, S, A])(f: A => B): Exchange[C, D, S, B] = {
      Exchange(fa.get, f compose fa.inverseGet)
    }
  }

  implicit final def profunctorExchange[E, F]: Profunctor[Exchange[E, F, *, *]] = new Profunctor[Exchange[E, F, *, *]] {
    override def dimap[A, B, C, D](fab: Exchange[E, F, A, B])(f: C => A)(g: B => D): Exchange[E, F, C, D] =
      Exchange(fab.get compose f, g compose fab.inverseGet)
  }
}

object Exchange extends ExchangeInstances