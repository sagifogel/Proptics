package proptics

import cats.Eq
import cats.syntax.eq._
import cats.syntax.option._
import cats.arrow.Profunctor

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
abstract class Iso[P[_, _] : Profunctor, S, T, A, B] extends Optic[P, S, T, A, B] { self =>
}

object Iso {
  private[proptics] def apply[P[_, _], S, T, A, B](f: P[A, B] => P[S, T])(implicit ev: Profunctor[P]): Iso[P, S, T, A, B] = new Iso[P, S, T, A, B] {
    override def apply(pab: P[A, B]): P[S, T] = f(pab)
  }

  def apply[P[_, _], S, T, A, B](get: S => A)(inverseGet: B => T)(implicit ev: Profunctor[P]): Iso[P, S, T, A, B] = {
    iso(get)(inverseGet)
  }

  def iso[P[_, _], S, T, A, B](get: S => A)(inverseGet: B => T)(implicit ev: Profunctor[P]): Iso[P, S, T, A, B] = {
    Iso(ev.dimap(_)(get)(inverseGet))
  }

  def curried[A, B, C, D, E, F](implicit ev: Profunctor[(*, *)]): Iso[(*, *), (A, B) => C, (D, E) => F, A => B => C, D => E => F] =
    iso[(*, *), (A, B) => C, (D, E) => F, A => B => C, D => E => F](_.curried)(F.uncurried[D, E, F])

  def uncurried[A, B, C, D, E, F](implicit ev: Profunctor[(*, *)]): Iso[(*, *), A => B => C, D => E => F, (A, B) => C, (D, E) => F] =
    iso[(*, *), A => B => C, D => E => F, (A, B) => C, (D, E) => F](F.uncurried[A, B, C])(_.curried)

  def flipped[A, B, C, D, E, F](implicit ev: Profunctor[* => *]): Iso[* => *, A => B => C, D => E => F, B => A => C, E => D => F] =
    iso[* => *, A => B => C, D => E => F, B => A => C, E => D => F](_.flip)(_.flip)
}

object Iso_ {
  private[proptics] def apply[P[_, _], S, A](f: P[A, A] => P[S, S])(implicit ev: Profunctor[P]): Iso_[P, S, A] = new Iso_[P, S, A] {
    override def apply(pab: P[A, A]): P[S, S] = f(pab)
  }

  def apply[P[_, _], S, A](get: S => A)(inverseGet: A => S)(implicit ev: Profunctor[P]): Iso_[P, S, A] = {
    Iso(get)(inverseGet)
  }

  /** If `A1` is obtained from `A` by removing a single value, then `Option[A1]` is isomorphic to `A` */
  def non[P[_, _], A](a: A)(implicit ev: Eq[A], ev2: Profunctor[P]): Iso_[P, Option[A], A] = {
    def g(a1: A): Option[A] = if (a1 === a) None else a.some

    Iso.iso((op: Option[A]) => op.getOrElse(a))(g)
  }
}
