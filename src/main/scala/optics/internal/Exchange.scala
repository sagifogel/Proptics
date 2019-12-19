package optics.internal

import cats.Functor
import cats.arrow.Profunctor

/** The [[Exchange]] profunctor characterizes an [[optics.Iso]] */
final case class Exchange[A, B, S, T](f: S => A, g: B => T)

abstract class ExchangeInstances {
  implicit final def functorExchange[C, D, S]: Functor[Exchange[C, D, S, *]] = new Functor[Exchange[C, D, S, *]] {
    override def map[A, B](fa: Exchange[C, D, S, A])(f: A => B): Exchange[C, D, S, B] = {
      Exchange(fa.f, f compose fa.g)
    }
  }

  implicit final def profunctorExchange[E, F]: Profunctor[Exchange[E, F, *, *]] = new Profunctor[Exchange[E, F, *, *]] {
    override def dimap[A, B, C, D](fab: Exchange[E, F, A, B])(f: C => A)(g: B => D): Exchange[E, F, C, D] =
      Exchange(fab.f compose f, g compose fab.g)
  }
}

object Exchange extends ExchangeInstances