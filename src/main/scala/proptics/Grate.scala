package proptics

import cats.instances.function._
import cats.syntax.eq._
import cats.syntax.option._
import cats.{Applicative, Comonad, Distributive, Eq, Functor, Monoid}
import proptics.internal.{Forget, Zipping}
import proptics.profunctor.{Closed, Costar}
import proptics.rank2types.Rank2TypeGrateLike
import proptics.syntax.FunctionSyntax._

import scala.Function.const

/**
 * <a href="http://r6research.livejournal.com/28050.html">A [[Grate]]</a>
 *
 * @tparam S the source of an [[Grate]]
 * @tparam T the modified source of an [[Grate]]
 * @tparam A the target of an [[Grate]]
 * @tparam B the modified target of an [[Grate]]
 */
abstract class Grate[S, T, A, B] { self =>
  def apply[P[_, _]](pab: P[A, B])(implicit ev: Closed[P]): P[S, T]

  def view(s: S)(implicit ev: Monoid[A]): A = self[Forget[A, *, *]](Forget(identity)).runForget(s)

  def set(b: B): S => T = over(const(b))

  def over(f: A => B): S => T = self(f)

  def overF[F[_]: Applicative](f: A => F[B])(s: S)(implicit ev: Monoid[A]): F[T] = traverse(s)(f)

  def traverse[F[_]](s: S)(f: A => F[B])(implicit ev0: Applicative[F], ev1: Monoid[A]): F[T] =
    ev0.map(f(self.view(s)))(self.set(_)(s))

  def filter(f: A => Boolean)(s: S)(implicit ev: Monoid[A]): Option[A] = view(s).some.filter(f)

  def exists(f: A => Boolean)(s: S)(implicit ev: Monoid[A]): Boolean = f(view(s))

  def noExists(f: A => Boolean)(s: S)(implicit ev: Monoid[A]): Boolean = !exists(f)(s)

  def contains(s: S)(a: A)(implicit ev0: Eq[A], ev1: Monoid[A]): Boolean = exists(_ === a)(s)

  def notContains(s: S)(a: A)(implicit ev0: Eq[A], ev1: Monoid[A]): Boolean = !contains(s)(a)

  def zipWith[F[_]](f: A => A => B): S => S => T = self(Zipping(f)).runZipping

  def zipWithF[F[_]: Comonad](fs: F[S])(f: F[A] => B): T = self(Costar(f)).runCostar(fs)
}

object Grate {
  private[proptics] def apply[S, T, A, B](f: Rank2TypeGrateLike[S, T, A, B]): Grate[S, T, A, B] = new Grate[S, T, A, B] {
    override def apply[P[_, _]](pab: P[A, B])(implicit ev: Closed[P]): P[S, T] = f(pab)
}

  def apply[S, T, A, B](to: ((S => A) => B) => T): Grate[S, T, A, B] = Grate(new Rank2TypeGrateLike[S, T, A, B] {
      override def apply[P[_, _]](pab: P[A, B])(implicit ev: Closed[P]): P[S, T] =
        ev.dimap[(S => A) => A, (S => A) => B, S, T](ev.closed(pab))(_.`#`)(to)
    })

  def cotraverse[F[_], A, B](implicit ev: Distributive[F]): Grate[F[A], F[B], A, B] = {
    def cotraverse[G[_]: Functor](f: G[A] => B)(gfa: G[F[A]]): F[B] =
      ev.map(ev.cosequence(gfa))(f)

    Grate[F[A], F[B], A, B](cotraverse(_: (F[A] => A) => B)(identity)(Functor[F[A] => *]))
  }
}

object Grate_ {
  def apply[S, A](to: ((S => A) => A) => S): Grate_[S, A] = Grate[S, S, A, A](to)

  def cotraverse[F[_]: Distributive, A]: Grate_[F[A], A] = Grate.cotraverse
}
