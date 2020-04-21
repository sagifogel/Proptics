package proptics

import cats.syntax.eq._
import cats.syntax.option._
import cats.{Applicative, Eq, Id, Monoid}
import proptics.internal.Grating

import scala.Function.const

/**
  * An Optic with fixed type [[Grating]] [[cats.arrow.Profunctor]]
  *
  * @tparam S the source of an [[AGrate_]]
  * @tparam T the modified source of an [[AGrate_]]
  * @tparam A the target of an [[AGrate_]]
  * @tparam B the modified target of an [[AGrate_]]
  */
abstract class AGrate_[S, T, A, B] { self =>
  def apply(grating: Grating[A, B, A, B]): Grating[A, B, S, T]

  def view(s: S)(implicit ev: Monoid[A]): A = asGrate_.view(s)

  def set(b: B)(s: S)(implicit ev: Monoid[A]): T = over(const(b))(s)

  def over(f: A => B)(s: S)(implicit ev: Monoid[A]): T = overF[Id](f)(s)

  def cotraverse[F[_]](fs: F[S])(f: F[A] => B)(implicit ev: Applicative[F]): T = runGrating(f compose ev.map(fs))

  def overF[F[_]: Applicative](f: A => F[B])(s: S)(implicit ev: Monoid[A]): F[T] = traverse(s)(f)

  def traverse[F[_]](s: S)(f: A => F[B])(implicit ev0: Applicative[F], ev1: Monoid[A]): F[T] =
    ev0.map(f(self.view(s)))(self.set(_)(s))

  def filter(f: A => Boolean)(s: S)(implicit ev: Monoid[A]): Option[A] = view(s).some.filter(f)

  def exists(f: A => Boolean)(s: S)(implicit ev: Monoid[A]): Boolean = f(view(s))

  def noExists(f: A => Boolean)(s: S)(implicit ev: Monoid[A]): Boolean = !exists(f)(s)

  def contains(s: S)(a: A)(implicit ev0: Eq[A], ev1: Monoid[A]): Boolean = exists(_ === a)(s)

  def notContains(s: S)(a: A)(implicit ev0: Eq[A], ev1: Monoid[A]): Boolean = !contains(s)(a)

  def asGrate_ : Grate_[S, T, A, B] = Grate_(withGrate _)

  def withGrate(f: (S => A) => B): T = runGrating(f)

  private[this] def runGrating: ((S => A) => B) => T = self(Grating(_.apply(identity))).runGrating
}

object AGrate_ {
  private[proptics] def apply[S, T, A, B](f: Grating[A, B, A, B] => Grating[A, B, S, T]): AGrate_[S, T, A, B] = new AGrate_[S, T, A, B] {
    override def apply(grating: Grating[A, B, A, B]): Grating[A, B, S, T] = f(grating)
  }

  def apply[S, T, A, B](to: ((S => A) => B) => T)(implicit ev: DummyImplicit): AGrate_[S, T, A, B] = AGrate_((_: Grating[A, B, A, B]) => Grating(to))
}

object AGrate {
  def apply[S, A](to: ((S => A) => A) => S): AGrate[S, A] = AGrate_(to)
}
