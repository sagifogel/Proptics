package proptics

import cats.data.Const
import cats.syntax.either._
import cats.syntax.eq._
import cats.{Alternative, Applicative, Eq, Monoid}
import proptics.newtype.First
import proptics.profunctor.{Choice, Star}
import proptics.rank2types.Rank2TypePrismLike

import scala.Function.const

/**
 * @tparam S the source of a [[Prism]]
 * @tparam T the modified source of a [[Prism]]
 * @tparam A the target of a [[Prism]]
 * @tparam B the modified target of a [[Prism]]
 */
abstract class Prism[S, T, A, B] { self =>
  private[proptics] def apply[P[_, _]](pab: P[A, B])(implicit ev: Choice[P]): P[S, T]

  def over(f: A => B): S => T = self(f)

  def set(b: B): S => T = over(const(b))

  def preview(s: S): Option[A] = foldMap(First[A] _ compose Some[A])(s).runFirst

  def overF[F[_]](f: A => F[B])(s: S)(implicit ev: Applicative[F]): F[T]

  private def foldMap[R: Monoid](f: A => R)(s: S): R = overF[Const[R, *]](Const[R, B] _ compose f)(s).getConst
}

object Prism {
  private[proptics] def apply[S, T, A, B](prismLike: Rank2TypePrismLike[S, T, A, B]): Prism[S, T, A, B] = new Prism[S, T, A, B] { self =>
    override def apply[P[_, _]](pab: P[A, B])(implicit ev: Choice[P]): P[S, T] = prismLike(pab)

    override def overF[F[_]](f: A => F[B])(s: S)(implicit ev: Applicative[F]): F[T] =
      prismLike[Star[F, *, *]](Star(f)).runStar(s)
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