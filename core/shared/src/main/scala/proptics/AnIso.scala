package proptics

import cats.arrow.Profunctor
import cats.syntax.eq._
import cats.syntax.option._
import cats.{Applicative, Eq, Functor, Id}
import proptics.internal.Exchange

import scala.Function.const

/**
 * An Iso with fixed type [[Exchange]] [[cats.arrow.Profunctor]]
 *
 * @tparam S the source of a [[AnIso_]]
 * @tparam T the modified source of an [[AnIso_]]
 * @tparam A the target of an [[AnIso_]]
 * @tparam B the modified target of an [[AnIso_]]
 */
abstract class AnIso_[S, T, A, B] { self =>
  private[proptics] def apply(exchange: Exchange[A, B, A, B]): Exchange[A, B, S, T]

  def view[R](s: S): A = self(Exchange(identity, identity)).get(s)

  def set(b: B): S => T = over(const(b))

  def over(f: A => B): S => T = overF[Id](f)

  def overF[F[_]: Applicative](f: A => F[B])(s: S): F[T] = traverse(s)(f)

  def traverse[F[_]](s: S)(f: A => F[B])(implicit ev: Applicative[F]): F[T] = ev.map(f(view(s)))(set(_)(s))

  def filter(f: A => Boolean): S => Option[A] = s => view(s).some.filter(f)

  def exists(f: A => Boolean): S => Boolean = f compose view

  def noExists(f: A => Boolean): S => Boolean = s => !exists(f)(s)

  def contains(s: S)(a: A)(implicit ev: Eq[A]): Boolean = exists(_ === a)(s)

  def notContains(s: S)(a: A)(implicit ev: Eq[A]): Boolean = !contains(s)(a)

  def asIso_ : Iso_[S, T, A, B] = self.withIso(Iso_[S, T, A, B])

  def withIso[P[_, _], R](f: (S => A) => (B => T) => R): R = {
    val exchange = self.apply(Exchange(identity, identity))

    f(exchange.get)(exchange.inverseGet)
  }

  def au[P[_, _], E](f: (B => T) => E => S): E => A =
    withIso(sa => bt => e => sa(f(bt)(e)))

  def auf[P[_, _], E, R](f: P[R, A] => E => B)(g: P[R, S])(implicit ev: Profunctor[P]): E => T =
    withIso(sa => bt => e => bt(f(ev.rmap(g)(sa))(e)))

  def under[P[_, _]](f: T => S): B => A = withIso(sa => bt => sa compose f compose bt)

  def mapping[P[_, _], F[_], G[_]](implicit ev0: Functor[F], ev1: Functor[G]): Iso_[F[S], G[T], F[A], G[B]] =
    withIso(sa => bt => Iso_(ev0.lift(sa))(ev1.lift(bt)))

  def dimapping[P[_, _], Q[_, _], SS, TT, AA, BB](that: AnIso_[SS, TT, AA, BB])(implicit ev0: Profunctor[P], ev1: Profunctor[Q]): Iso_[P[A, SS], Q[B, TT], P[S, AA], Q[T, BB]] =
    withIso[P, Iso_[P[A, SS], Q[B, TT], P[S, AA], Q[T, BB]]](sa => bt => that.withIso[Q, Iso_[P[A, SS], Q[B, TT], P[S, AA], Q[T, BB]]](ssaa => bbtt => {
      Iso_.iso[P[A, SS], Q[B, TT], P[S, AA], Q[T, BB]](ev0.dimap(_)(sa)(ssaa))(ev1.dimap(_)(bt)(bbtt))
    }))
}

object AnIso_ {
  private[proptics] def apply[S, T, A, B](f: Exchange[A, B, A, B] => Exchange[A, B, S, T]): AnIso_[S, T, A, B] = new AnIso_[S, T, A, B] { self =>
    override def apply(exchange: Exchange[A, B, A, B]): Exchange[A, B, S, T] = f(exchange)
  }

  def apply[S, T, A, B](get: S => A)(inverseGet: B => T): AnIso_[S, T, A, B] =
    AnIso_((ex: Exchange[A, B, A, B]) => Exchange(ex.get compose get, inverseGet compose ex.inverseGet))
}

object AnIso {
  def apply[S, A](get: S => A)(inverseGet: A => S): AnIso[S, A] = AnIso_(get)(inverseGet)
}
