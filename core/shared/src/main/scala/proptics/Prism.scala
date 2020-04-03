package proptics

import cats.data.Const
import cats.syntax.option._
import cats.syntax.either._
import cats.syntax.eq._
import cats.{Alternative, Applicative, Comonad, Eq, Monoid}
import proptics.internal.{Tagged, Zipping}
import proptics.instances.BooleanInstances._
import proptics.newtype.{Disj, First, Newtype}
import proptics.profunctor.{Choice, Costar, Star}
import proptics.rank2types.Rank2TypePrismLike

import scala.Function.const

/**
 * @tparam S the source of a [[Prism_]]
 * @tparam T the modified source of a [[Prism_]]
 * @tparam A the target of a [[Prism_]]
 * @tparam B the modified target of a [[Prism_]]
 */
abstract class Prism_[S, T, A, B] extends Serializable { self =>
  private[proptics] def apply[P[_, _]](pab: P[A, B])(implicit ev: Choice[P]): P[S, T]

  def preview(s: S): Option[A] = foldMapNewtype[First[A], Option[A]](_.some)(s)

  def review(b: B): T = self(Tagged[A, B](b)).runTag

  def set(b: B): S => T = over(const(b))

  def setOption(b: B): S => Option[T] = overOption(const(b))

  def over(f: A => B): S => T = self(f)

  def overOption(f: A => B): S => Option[T] = s => preview(s).map(review _ compose f)

  def overF[F[_]: Applicative](f: A => F[B])(s: S): F[T] = traverse(s)(f)

  def traverse[F[_]: Applicative](s: S)(f: A => F[B]): F[T] = self[Star[F, *, *]](Star(f)).runStar(s)

  def isEmpty(s: S): Boolean = preview(s).isEmpty

  def nonEmpty(s: S): Boolean = !isEmpty(s)

  def exists(f: A => Boolean): S => Boolean = foldMapNewtype[Disj[Boolean], Boolean](f)

  def notExists(f: A => Boolean): S => Boolean = s => !exists(f)(s)

  def contains(s: S)(a: A)(implicit ev: Eq[A]): Boolean = exists(_ === a)(s)

  def notContains(s: S)(a: A)(implicit ev: Eq[A]): Boolean = !contains(s)(a)

  def filter(p: A => Boolean): S => Option[A] = preview(_).filter(p)

  def forall(p: A => Boolean): S => Boolean = preview(_).forall(p)

  def zipWith[F[_]](f: A => A => B): S => S => T = self(Zipping(f)).runZipping

  def zipWithF[F[_]: Comonad : Applicative](fs: F[S])(f: F[A] => B): T = self(Costar(f)).runCostar(fs)

  private def foldMapNewtype[F: Monoid, R](f: A => R)(s: S)(implicit ev: Newtype.Aux[F, R]): R =
    ev.unwrap(foldMap(s)(ev.wrap _ compose f))

  private def foldMap[R: Monoid](s: S)(f: A => R): R = overF[Const[R, *]](Const[R, B] _ compose f)(s).getConst
}

object Prism_ {
  private[proptics] def apply[S, T, A, B](prismLike: Rank2TypePrismLike[S, T, A, B]): Prism_[S, T, A, B] = new Prism_[S, T, A, B] { self =>
    override def apply[P[_, _]](pab: P[A, B])(implicit ev: Choice[P]): P[S, T] = prismLike(pab)
  }

  def apply[S, T, A, B](to: B => T)(from: S => Either[T, A]): Prism_[S, T, A, B] = {
    Prism_(new Rank2TypePrismLike[S, T, A, B] {
      override def apply[P[_, _]](pab: P[A, B])(implicit ev: Choice[P]): P[S, T] = {
        val right = ev.right[T, A, T](ev.rmap(pab)(to))

        ev.dimap(right)(from)(_.fold(identity, identity))
      }
    })
  }
}

object Prism {
  def fromOption[S, A](to: A => S)(from: S => Option[A]): Prism[S, A] =
    Prism(to)(s => from(s).fold(s.asLeft[A])(_.asRight[S]))

  def apply[S, A](to: A => S)(from: S => Either[S, A]): Prism[S, A] = Prism_(to)(from)

  def nearly[A](a: A)(predicate: A => Boolean)(implicit ev: Alternative[Option]): Prism[A, Unit] =
    Prism.fromOption[A, Unit](const(a))(ev.guard _ compose predicate)

  def only[A: Eq](a: A)(implicit ev: Alternative[Option]): Prism[A, Unit] = nearly(a)(_ === a)
}