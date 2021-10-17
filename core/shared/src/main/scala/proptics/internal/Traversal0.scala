package proptics.internal

import cats.data.Const
import cats.{Applicative, Monoid}

private[proptics] trait Traversal0[S, T, A, B] extends Setter0[S, T, A, B] {
  protected def foldMap[R: Monoid](s: S)(f: A => R): R = overF[Const[R, *]](Const[R, B] _ compose f)(s).getConst

  /** synonym for [[traverse]], flipped */
  final def overF[F[_]: Applicative](f: A => F[B])(s: S): F[T] = traverse(s)(f)

  /** modify each focus of a Traversal using a Functor, resulting in a change of type to the full structure */
  def traverse[F[_]: Applicative](s: S)(f: A => F[B]): F[T]
}
