package proptics.internal

import cats.Functor
import cats.arrow.Profunctor

/** The Exchange profunctor characterizes an [[proptics.Iso_]] */
final case class Exchange[A, B, S, T](view: S => A, review: B => T) { self =>
  def compose[C, D](other: Exchange[C, D, A, B]): Exchange[C, D, S, T] =
    Exchange(other.view compose self.view, self.review compose other.review)
}

abstract class ExchangeInstances {
  implicit final def functorExchange[C, D, S]: Functor[Exchange[C, D, S, *]] = new Functor[Exchange[C, D, S, *]] {
    override def map[A, B](fa: Exchange[C, D, S, A])(f: A => B): Exchange[C, D, S, B] =
      Exchange(fa.view, f compose fa.review)
  }

  implicit final def profunctorExchange[E, F]: Profunctor[Exchange[E, F, *, *]] = new Profunctor[Exchange[E, F, *, *]] {
    override def dimap[A, B, C, D](fab: Exchange[E, F, A, B])(f: C => A)(g: B => D): Exchange[E, F, C, D] =
      Exchange(fab.view compose f, g compose fab.review)
  }
}

object Exchange extends ExchangeInstances