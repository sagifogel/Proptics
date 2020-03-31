package proptics

import cats.data.Const
import cats.syntax.either._
import cats.syntax.eq._
import cats.syntax.option._
import cats.{Applicative, Eq, Id, Monoid}
import proptics.instances.BooleanInstances._
import proptics.internal.Market
import proptics.newtype.{Disj, First, Newtype}

import scala.Function.const

/**
 * * A [[Prism]] with fixed type [[Market]] [[cats.arrow.Profunctor]]
 *
 * @tparam S the source of an [[APrism]]
 * @tparam T the modified source of an [[APrism]]
 * @tparam A the target of an [[APrism]]
 * @tparam B the modified target of an [[APrism]]
 */
abstract class APrism[S, T, A, B] { self =>
  private[proptics] def apply(market: Market[A, B, A, B]): Market[A, B, S, T]

  def preview(s: S): Option[A] = foldMapNewtype[First[A], Option[A]](_.some)(s)

  def review(b: B): T = self(Market(identity, _.asRight[B])).to(b)

  def set(b: B): S => T = over(const(b))

  def setOption(b: B): S => Option[T] = overOption(const(b))

  def over(f: A => B): S => T = overF[Id](f)

  def overOption(f: A => B): S => Option[T] = s => preview(s).map(review _ compose f)

  def overF[F[_]: Applicative](f: A => F[B])(s: S): F[T] = traverse(s)(f)

  def traverse[F[_]](s: S)(f: A => F[B])(implicit ev: Applicative[F]): F[T]

  def isEmpty(s: S): Boolean = preview(s).isEmpty

  def nonEmpty(s: S): Boolean = !isEmpty(s)

  def exists(f: A => Boolean): S => Boolean = foldMapNewtype[Disj[Boolean], Boolean](f)

  def notExists(f: A => Boolean): S => Boolean = s => !exists(f)(s)

  def contains(s: S)(a: A)(implicit ev: Eq[A]): Boolean = exists(_ === a)(s)

  def notContains(s: S)(a: A)(implicit ev: Eq[A]): Boolean = !contains(s)(a)

  def filter(p: A => Boolean): S => Option[A] = preview(_).filter(p)

  def forall(p: A => Boolean): S => Boolean = preview(_).forall(p)

  def withPrism[R](f: (B => T) => (S => Either[T, A]) => R): R = {
    val market = self(Market(identity, _.asRight[B]))

    f(market.to)(market.from)
  }

  def matching(s: S): Either[T, A] = withPrism(const(_.apply(s)))

  def asPrism[P[_, _]]: Prism[S, T, A, B] = self.withPrism(Prism[S, T, A, B])

  private def foldMapNewtype[F: Monoid, R](f: A => R)(s: S)(implicit ev: Newtype.Aux[F, R]): R =
    ev.unwrap(foldMap(s)(ev.wrap _ compose f))

  private def foldMap[R: Monoid](s: S)(f: A => R): R = overF[Const[R, *]](Const[R, B] _ compose f)(s).getConst
}

object APrism {
  def apply[S, T, A, B](to: B => T)(from: S => Either[T, A]): APrism[S, T, A, B] = new APrism[S, T, A, B] { self =>
    override private[proptics] def apply(market: Market[A, B, A, B]): Market[A, B, S, T] = Market(to, from)

    override def traverse[F[_]](s: S)(f: A => F[B])(implicit ev: Applicative[F]): F[T] = from(s) match {
        case Right(a) => ev.map(f(a))(to)
        case Left(t) => ev.pure(t)
    }
  }
}

object APrism_ {
  def apply[S, A](to: A => S)(from: S => Either[S, A]): APrism_[S, A] = APrism(to)(from)
}