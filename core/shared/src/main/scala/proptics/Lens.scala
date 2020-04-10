package proptics

import cats.arrow.Strong
import cats.instances.function._
import cats.syntax.apply._
import cats.syntax.eq._
import cats.syntax.option._
import cats.{Alternative, Comonad, Eq, Functor}
import proptics.internal.{Forget, Wander, Zipping}
import proptics.newtype.Disj
import proptics.profunctor.{Costar, Star}
import proptics.rank2types.Rank2TypeLensLike

import scala.Function.const

/**
  * Given a type whose "focus element" always exists,
  * a [[Lens_]] provides a convenient way to view, set, and transform
  * that element.
  *
  * @tparam S the source of a [[Lens_]]
  * @tparam T the modified source of a [[Lens_]]
  * @tparam A the target of a [[Lens_]]
  * @tparam B the modified target of a [[Lens_]]
  */
abstract class Lens_[S, T, A, B] extends Serializable { self =>
  private[proptics] def apply[P[_, _]](pab: P[A, B])(implicit ev: Strong[P]): P[S, T]

  def view(s: S): A = self[Forget[A, *, *]](Forget(identity)).runForget(s)

  def set(b: B): S => T = over(const(b))

  def over(f: A => B): S => T = self(f)

  def overF[F[_]: Functor](f: A => F[B])(s: S): F[T] = traverse(s)(f)

  def traverse[F[_]: Functor](s: S)(f: A => F[B]): F[T] = self(Star(f)).runStar(s)

  def filter(f: A => Boolean): S => Option[A] = s => view(s).some.filter(f)

  def exists(f: A => Boolean): S => Boolean = f compose view

  def noExists(f: A => Boolean): S => Boolean = s => !exists(f)(s)

  def contains(s: S)(a: A)(implicit ev: Eq[A]): Boolean = exists(_ === a)(s)

  def notContains(s: S)(a: A)(implicit ev: Eq[A]): Boolean = !contains(s)(a)

  def failover[F[_]](f: A => B)(s: S)(implicit ev0: Strong[Star[(Disj[Boolean], *), *, *]], ev1: Alternative[F]): F[T] = {
    val star = Star[(Disj[Boolean], *), A, B](a => (Disj(true), f(a)))

    self(star).runStar(s) match {
      case (Disj(true), x)  => ev1.pure(x)
      case (Disj(false), _) => ev1.empty
    }
  }

  def zipWith[F[_]](f: A => A => B): S => S => T = self(Zipping(f)).runZipping

  def zipWithF[F[_]: Comonad](fs: F[S])(f: F[A] => B): T = self(Costar(f)).runCostar(fs)

  def compose[C, D](other: Lens_[A, B, C, D]): Lens_[S, T, C, D] = new Lens_[S, T, C, D] {
    override private[proptics] def apply[P[_, _]](pab: P[C, D])(implicit ev: Strong[P]): P[S, T] = self(other(pab))
  }

  def compose[C, D](other: Iso_[A, B, C, D]): Lens_[S, T, C, D] = new Lens_[S, T, C, D] {
    override private[proptics] def apply[P[_, _]](pab: P[C, D])(implicit ev: Strong[P]): P[S, T] = self(other(pab))
  }

  def compose[C, D](other: Prism_[A, B, C, D]): Traversal_[S, T, C, D] = new Traversal_[S, T, C, D] {
    override private[proptics] def apply[P[_, _]](pab: P[C, D])(implicit ev: Wander[P]) = self(other(pab))
  }
}

object Lens_ {
  private[proptics] def apply[S, T, A, B](f: Rank2TypeLensLike[S, T, A, B]): Lens_[S, T, A, B] = new Lens_[S, T, A, B] { self =>
    override def apply[P[_, _]](pab: P[A, B])(implicit ev: Strong[P]): P[S, T] = f(pab)
  }

  /**
    * Create a [[Lens_]] from a getter/setter pair.
    */
  def apply[S, T, A, B](get: S => A)(set: S => B => T): Lens_[S, T, A, B] = Lens_((get, set).mapN(Tuple2.apply))

  def apply[S, T, A, B](to: S => (A, B => T)): Lens_[S, T, A, B] =
    Lens_(new Rank2TypeLensLike[S, T, A, B] {
      override def apply[P[_, _]](pab: P[A, B])(implicit ev: Strong[P]): P[S, T] =
        liftOptic(to)(ev)(pab)
    })

  private[proptics] def liftOptic[P[_, _], S, T, A, B](to: S => (A, B => T))(implicit ev: Strong[P]): P[A, B] => P[S, T] =
    pab => ev.dimap(ev.first[A, B, B => T](pab))(to) { case (b, f) => f(b) }
}

object Lens {
  def apply[S, A](get: S => A)(set: S => A => S): Lens[S, A] = Lens_[S, S, A, A](get)(set)

  def apply[S, A](to: S => (A, A => S)): Lens[S, A] = Lens_[S, S, A, A](to)
}
