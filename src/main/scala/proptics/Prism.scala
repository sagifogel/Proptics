package proptics

import cats.data.Const
import cats.syntax.option._
import cats.syntax.either._
import cats.syntax.eq._
import cats.{Alternative, Applicative, Eq, Monoid}
import proptics.internal.Tagged
import proptics.instances.BooleanInstances._
import proptics.newtype.{Disj, First, Newtype}
import proptics.profunctor.{Choice, Star}
import proptics.rank2types.Rank2TypePrismLike

import scala.Function.const

/**
 * @tparam S the source of a [[Prism]]
 * @tparam T the modified source of a [[Prism]]
 * @tparam A the target of a [[Prism]]
 * @tparam B the modified target of a [[Prism]]
 */
abstract class Prism[S, T, A, B] extends Serializable { self =>
  private[proptics] def apply[P[_, _]](pab: P[A, B])(implicit ev: Choice[P]): P[S, T]

  def preview(s: S): Option[A] = foldMapNewtype[First[A], Option[A]](_.some)(s)

  def review(b: B): T = self(Tagged[A, B](b)).runTag

  def set(b: B): S => T = over(const(b))

  def setOption(b: B): S => Option[T] = overOption(const(b))

  def over(f: A => B): S => T = self(f)

  def overOption(f: A => B): S => Option[T] = s => preview(s).map(review _ compose f)

  def overF[F[_]](f: A => F[B])(s: S)(implicit ev: Applicative[F]): F[T] = self[Star[F, *, *]](Star(f)).runStar(s)

  def isEmpty(s: S): Boolean = preview(s).isEmpty

  def nonEmpty(s: S): Boolean = !isEmpty(s)

  def exists(f: A => Boolean): S => Boolean = foldMapNewtype[Disj[Boolean], Boolean](f)

  def notExists(f: A => Boolean): S => Boolean = s => !exists(f)(s)

  def contains(s: S)(a: A)(implicit ev: Eq[A]): Boolean = exists(_ === a)(s)

  def notContains(s: S)(a: A)(implicit ev: Eq[A]): Boolean = !contains(s)(a)

  def filter(p: A => Boolean): S => Option[A] = preview(_).filter(p)

  def forall(p: A => Boolean): S => Boolean = preview(_).forall(p)

  private def foldMapNewtype[F: Monoid, R](f: A => R)(s: S)(implicit ev: Newtype.Aux[F, R]): R =
    ev.unwrap(foldMap(s)(ev.wrap _ compose f))

  private def foldMap[R: Monoid](s: S)(f: A => R): R = overF[Const[R, *]](Const[R, B] _ compose f)(s).getConst
}

object Prism {
  private[proptics] def apply[S, T, A, B](prismLike: Rank2TypePrismLike[S, T, A, B]): Prism[S, T, A, B] = new Prism[S, T, A, B] { self =>
    override def apply[P[_, _]](pab: P[A, B])(implicit ev: Choice[P]): P[S, T] = prismLike(pab)
  }

  def apply[S, T, A, B](to: B => T)(from: S => Either[T, A]): Prism[S, T, A, B] = {
    Prism(new Rank2TypePrismLike[S, T, A, B] {
      override def apply[P[_, _]](pab: P[A, B])(implicit ev: Choice[P]): P[S, T] = {
        val right = ev.right[T, A, T](ev.rmap(pab)(to))

        ev.dimap(right)(from)(_.fold(identity, identity))
      }
    })
  }
}

object Prism_ {
  def apply[S, A](to: A => S)(from: S => Option[A]): Prism_[S, A] =
    prism(to)(s => from(s).fold(s.asLeft[A])(_.asRight[S]))

  def prism[S, A](to: A => S)(from: S => Either[S, A]): Prism_[S, A] = Prism(to)(from)

  def nearly[A](a: A)(predicate: A => Boolean)(implicit ev: Alternative[Option]): Prism_[A, Unit] =
    Prism_[A, Unit](const(a))(ev.guard _ compose predicate)

  def only[A: Eq](a: A)(implicit ev: Alternative[Option]): Prism_[A, Unit] = nearly(a)(_ === a)
}