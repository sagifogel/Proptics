package proptics

import Fold._
import cats.{Foldable, Monoid}
import proptics.internal.Forget
import proptics.rank2types.Rank2TypeFoldLike

/**
 * A [[Getter]] is a [[Fold]]
 *
 * @tparam S the source of a [[Getter]]
 * @tparam T the modified source of a [[Getter]]
 * @tparam A the target of a [[Getter]]
 * @tparam B the modified target of a [[Getter]]
 */
abstract class Getter[S, T, A, B] extends Fold[S, T, A, B]

object Getter {
  def apply[S, T, A, B](f: Rank2TypeFoldLike[S, T, A, B]): Getter[S, T, A, B] = new Getter[S, T, A, B] {
    override def apply[R: Monoid](forget: Forget[R, A, B]): Forget[R, S, T] = f(forget)
  }

  def apply[S, T, A, B](f: S => A): Getter[S, T, A, B] = Getter(fromGetRank2TypeFoldLike[S, T, A, B](f))

  def fromFoldable[F[_], A, B, T](implicit ev0: Foldable[F]): Getter[F[A], B, A, T] = new Getter[F[A], B, A, T] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, A, T]): Forget[R, F[A], B] =
      Forget[R, F[A], B](ev0.foldMap(_)(forget.runForget))

    override def foldMap[R](s: F[A])(f: A => R)(implicit ev: Monoid[R]): R = ev0.foldMap(s)(f)
  }

  def replicate[A, B, T](i: Int): Getter[A, B, A, T] = Getter(replicateRank2TypeFoldLike[A, B, T](i))

  def unfolded[S, T, A, B](f: S => Option[(A, S)]): Getter[S, T, A, B] = Getter(unfoldedRank2TypeFoldLike[S, T, A, B](f))
}

object Getter_ {
  def apply[S, A](f: S => A): Getter_[S, A] = Getter[S, S, A, A](f)

  def fromFoldable[F[_], A, T](implicit ev0: Foldable[F]): Getter[F[A], A, A, T] = Getter.fromFoldable

  def replicate[A, B, T](i: Int): Getter[A, B, A, T] = Getter.replicate(i)

  def unfolded[S, T, A, B](f: S => Option[(A, S)]): Getter[S, T, A, B] = Getter.unfolded(f)
}
