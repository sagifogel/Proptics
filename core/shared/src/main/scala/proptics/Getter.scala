package proptics

import Fold_._
import cats.{Foldable, Monoid}
import proptics.internal.Forget
import proptics.rank2types.Rank2TypeFoldLike

/**
 * A [[Getter_]] is a [[Fold]]
 *
 * @tparam S the source of a [[Getter_]]
 * @tparam T the modified source of a [[Getter_]]
 * @tparam A the target of a [[Getter_]]
 * @tparam B the modified target of a [[Getter_]]
 */
abstract class Getter_[S, T, A, B] extends Fold_[S, T, A, B]

object Getter_ {
  def apply[S, T, A, B](f: Rank2TypeFoldLike[S, T, A, B]): Getter_[S, T, A, B] = new Getter_[S, T, A, B] {
    override def apply[R: Monoid](forget: Forget[R, A, B]): Forget[R, S, T] = f(forget)
  }

  def apply[S, T, A, B](get: S => A): Getter_[S, T, A, B] = Getter_(fromGetRank2TypeFoldLike[S, T, A, B](get))

  def fromFoldable[F[_], A, B, T](implicit ev0: Foldable[F]): Getter_[F[A], B, A, T] = new Getter_[F[A], B, A, T] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, A, T]): Forget[R, F[A], B] =
      Forget[R, F[A], B](ev0.foldMap(_)(forget.runForget))

    override def foldMap[R](s: F[A])(f: A => R)(implicit ev: Monoid[R]): R = ev0.foldMap(s)(f)
  }

  def replicate[A, B, T](i: Int): Getter_[A, B, A, T] = Getter_(replicateRank2TypeFoldLike[A, B, T](i))

  def unfolded[S, T, A, B](f: S => Option[(A, S)]): Getter_[S, T, A, B] = Getter_(unfoldRank2TypeFoldLike[S, T, A, B](f))
}

object Getter {
  def apply[S, A](f: S => A): Getter[S, A] = Getter_[S, S, A, A](f)

  def fromFoldable[F[_], A, T](implicit ev0: Foldable[F]): Getter_[F[A], A, A, T] = Getter_.fromFoldable

  def replicate[A, B, T](i: Int): Getter_[A, B, A, T] = Getter_.replicate(i)

  def unfolded[S, T, A, B](f: S => Option[(A, S)]): Getter_[S, T, A, B] = Getter_.unfolded(f)
}
