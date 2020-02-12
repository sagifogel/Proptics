package proptics

import cats.Eq
import cats.syntax.eq._
import cats.syntax.option._
import cats.arrow.Profunctor
import proptics.rank2types.Rank2TypeIsoLike

import scala.{Function => F}
import proptics.syntax.FunctionSyntax._

/**
 * A generalized isomorphism
 *
 * @tparam P an evidence of [[Profunctor]]
 * @tparam S the source of an [[Iso]]
 * @tparam T the modified source of an [[Iso]]
 * @tparam A the target of a [[Iso]]
 * @tparam B the modified target of a [[Iso]]
 */
abstract class Iso[S, T, A, B] { self =>
   def apply[P[_, _]](pab: P[A, B])(implicit ev: Profunctor[P]): P[S, T]
}

object Iso {
  private[proptics] def apply[S, T, A, B](f: Rank2TypeIsoLike[S, T, A, B]): Iso[S, T, A, B] = new Iso[S, T, A, B] {
    override def apply[P[_, _]](pab: P[A, B])(implicit ev: Profunctor[P]): P[S, T] = f(pab)
  }

  def apply[S, T, A, B](get: S => A)(inverseGet: B => T): Iso[S, T, A, B] = {
    iso(get)(inverseGet)
  }

  def iso[S, T, A, B](get: S => A)(inverseGet: B => T): Iso[S, T, A, B] = {
    Iso(new Rank2TypeIsoLike[S, T, A, B] {
      override def apply[P[_, _]](pab: P[A, B])(implicit ev: Profunctor[P]): P[S, T] =
        ev.dimap(pab)(get)(inverseGet)
    })
  }

  def curried[A, B, C, D, E, F]: Iso[(A, B) => C, (D, E) => F, A => B => C, D => E => F] =
    iso[(A, B) => C, (D, E) => F, A => B => C, D => E => F](_.curried)(F.uncurried[D, E, F])

  def uncurried[A, B, C, D, E, F]: Iso[A => B => C, D => E => F, (A, B) => C, (D, E) => F] =
    iso[A => B => C, D => E => F, (A, B) => C, (D, E) => F](F.uncurried[A, B, C])(_.curried)

  def flipped[A, B, C, D, E, F]: Iso[A => B => C, D => E => F, B => A => C, E => D => F] =
    iso[A => B => C, D => E => F, B => A => C, E => D => F](_.flip)(_.flip)
}

object Iso_ {
  private[proptics] def apply[S, A](f: Rank2TypeIsoLike[S, S, A, A]): Iso_[S, A] = new Iso_[S, A] {
    override def apply[P[_, _]](pab: P[A, A])(implicit ev: Profunctor[P]): P[S, S] = f(pab)
  }

  def apply[S, A](get: S => A)(inverseGet: A => S): Iso_[S, A] = {
    Iso(get)(inverseGet)
  }

  /** If `A1` is obtained from `A` by removing a single value, then `Option[A1]` is isomorphic to `A` */
  def non[P[_, _], A](a: A)(implicit ev: Eq[A]): Iso_[Option[A], A] = {
    def g(a1: A): Option[A] = if (a1 === a) None else a.some

    Iso.iso((op: Option[A]) => op.getOrElse(a))(g)
  }
}
