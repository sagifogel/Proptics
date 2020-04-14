package proptics

import cats.instances.function._
import cats.syntax.eq._
import cats.syntax.option._
import cats.{Applicative, Comonad, Distributive, Eq, Functor, Monoid}
import proptics.internal.{Forget, Grating, Tagged, Zipping}
import proptics.profunctor.{Closed, Costar}
import proptics.rank2types.Rank2TypeGrateLike
import proptics.syntax.FunctionSyntax._

import scala.Function.const

/**
  * <a href="http://r6research.livejournal.com/28050.html">A [[Grate]]</a>
  *
  * @tparam S the source of an [[Grate_]]
  * @tparam T the modified source of an [[Grate_]]
  * @tparam A the target of an [[Grate_]]
  * @tparam B the modified target of an [[Grate_]]
  */
abstract class Grate_[S, T, A, B] { self =>
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

  def compose[C, D](other: Iso_[A, B, C, D]): AGrate_[S, T, C, D] = new AGrate_[S, T, C, D] {
    override def apply(grating: Grating[C, D, C, D]): Grating[C, D, S, T] = self(other(grating))
  }

  def compose[C, D](other: AnIso_[A, B, C, D]): AGrate_[S, T, C, D] = self compose other.asIso_

  def compose[C, D](other: Setter_[A, B, C, D]): Setter_[S, T, C, D]= new Setter_[S, T, C, D] {
    override private[proptics] def apply(pab: C => D) = self(other(pab))
  }

  def compose[C, D](other: Getter_[A, B, C, D]): Fold_[S, T, C, D]= self compose other.asFold_

  def compose[C, D](other: Fold_[A, B, C, D]): Fold_[S, T, C, D]= new Fold_[S, T, C, D] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, C, D]) = self(other(forget))
  }

  def compose[C, D](other: Grate_[A, B, C, D]): Grate_[S, T, C, D] = new Grate_[S, T, C, D] {
    override def apply[P[_, _]](pab: P[C, D])(implicit ev: Closed[P]): P[S, T] = self(other(pab))
  }

  def compose[C, D](other: AGrate_[A, B, C, D]): AGrate_[S, T, C, D] = new AGrate_[S, T, C, D] {
    override def apply(grating: Grating[C, D, C, D]): Grating[C, D, S, T] = self(other(grating))
  }

  def compose[C, D](other: Review_[A, B, C, D]): Review_[S, T, C, D] = new Review_[S, T, C, D] {
    override private[proptics] def apply(tagged: Tagged[C, D]) = self(other(tagged))
  }
}

object Grate_ {
  private[proptics] def apply[S, T, A, B](f: Rank2TypeGrateLike[S, T, A, B]): Grate_[S, T, A, B] = new Grate_[S, T, A, B] {
    override def apply[P[_, _]](pab: P[A, B])(implicit ev: Closed[P]): P[S, T] = f(pab)
  }

  def apply[S, T, A, B](to: ((S => A) => B) => T): Grate_[S, T, A, B] =
    Grate_(new Rank2TypeGrateLike[S, T, A, B] {
      override def apply[P[_, _]](pab: P[A, B])(implicit ev: Closed[P]): P[S, T] =
        ev.dimap[(S => A) => A, (S => A) => B, S, T](ev.closed(pab))(_.`#`)(to)
    })

  def cotraverse[F[_], A, B](implicit ev: Distributive[F]): Grate_[F[A], F[B], A, B] = {
    def cotraverse[G[_]: Functor](f: G[A] => B)(gfa: G[F[A]]): F[B] =
      ev.map(ev.cosequence(gfa))(f)

    Grate_[F[A], F[B], A, B](cotraverse(_: (F[A] => A) => B)(identity)(Functor[F[A] => *]))
  }
}

object Grate {
  def apply[S, A](to: ((S => A) => A) => S): Grate[S, A] = Grate_[S, S, A, A](to)

  def cotraverse[F[_]: Distributive, A]: Grate[F[A], A] = Grate_.cotraverse
}
