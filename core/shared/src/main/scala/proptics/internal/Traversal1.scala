package proptics.internal

import cats.data.Const
import cats.syntax.apply._
import cats.{Applicative, Monoid}

private[proptics] trait Traversal1[S, T, A, B] extends Traversal0[S, T, A, B] with Fold1[S, A] {
  /** synonym for fold */
  final def view(s: S)(implicit ev: Monoid[A]): A = fold(s)

  /** map each focus of a Traversal to a [[cats.Monoid]], and combine the results */
  final override def foldMap[R: Monoid](s: S)(f: A => R): R = overF[Const[R, *]](Const[R, B] _ compose f)(s).getConst

  /** evaluate each  focus of a Traversal from left to right, and ignore the results structure */
  final def sequence_[F[_]](s: S)(implicit ev: Applicative[F]): F[Unit] = traverse_(s)(ev.pure)

  /** map each focus of a Traversal to an effect, from left to right, and ignore the results */
  final def traverse_[F[_], R](s: S)(f: A => F[R])(implicit ev: Applicative[F]): F[Unit] =
    foldLeft[F[Unit]](s)(ev.pure(()))((b, a) => ev.void(f(a)) *> b)
}
