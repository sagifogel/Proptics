package proptics

import cats.instances.function._
import cats.{Applicative, Distributive, Functor}
import proptics.internal.{Tagged, Zipping}
import proptics.profunctor.{Closed, Costar}
import proptics.rank2types.Rank2TypeGrateLike
import proptics.syntax.FunctionSyntax._

import scala.Function.const

/**
  * [[Grate_]] allows a generalized zipWith operation
  * <a href="http://r6research.livejournal.com/28050.html">A [[Grate]]</a>
  *
  * @tparam S the source of an [[Grate_]]
  * @tparam T the modified source of an [[Grate_]]
  * @tparam A the target of an [[Grate_]]
  * @tparam B the modified target of an [[Grate_]]
  */
abstract class Grate_[S, T, A, B] { self =>
  def apply[P[_, _]](pab: P[A, B])(implicit ev: Closed[P]): P[S, T]

  def review(b: B): T = self(Tagged[A, B](b)).runTag

  def set(b: B): S => T = over(const(b))

  def over(f: A => B): S => T = self(f)

  def cotraverse[F[_]: Applicative](fs: F[S])(f: F[A] => B): T = self(Costar(f)).runCostar(fs)

  def zipWith[F[_]](f: A => A => B): S => S => T = self(Zipping(f)).runZipping

  def zipWithF[F[_]: Applicative](fs: F[S])(f: F[A] => B): T = cotraverse(fs)(f)

  def compose[C, D](other: Iso_[A, B, C, D]): Grate_[S, T, C, D] = new Grate_[S, T, C, D] {
    override def apply[P[_, _]](pab: P[C, D])(implicit ev: Closed[P]): P[S, T] = self(other(pab))
  }

  def compose[C, D](other: AnIso_[A, B, C, D]): Grate_[S, T, C, D] = self compose other.asIso_

  def compose[C, D](other: Setter_[A, B, C, D]): Setter_[S, T, C, D] = new Setter_[S, T, C, D] {
    override private[proptics] def apply(pab: C => D) = self(other(pab))
  }

  def compose[C, D](other: Grate_[A, B, C, D]): Grate_[S, T, C, D] = new Grate_[S, T, C, D] {
    override def apply[P[_, _]](pab: P[C, D])(implicit ev: Closed[P]): P[S, T] = self(other(pab))
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
