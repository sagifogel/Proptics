package proptics

import cats.syntax.either._
import cats.syntax.eq._
import cats.{Alternative, Eq}
import proptics.profunctor.Choice
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
}

object Prism {
  private[proptics] def apply[S, T, A, B](f: Rank2TypePrismLike[S, T, A, B]): Prism[S, T, A, B] = new Prism[S, T, A, B] {
    override def apply[P[_, _]](pab: P[A, B])(implicit ev: Choice[P]): P[S, T] = f(pab)
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

  def only[A](a: A)(implicit ev: Alternative[Option], ev3: Eq[A]): Prism_[A, Unit] = nearly(a)(_ === a)
}