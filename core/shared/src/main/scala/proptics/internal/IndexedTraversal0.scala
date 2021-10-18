package proptics.internal

import cats.data.Const
import cats.{Applicative, Monoid}

trait IndexedTraversal0[I, S, T, A, B] extends IndexedSetter0[I, S, T, A, B] {
  protected def foldMap[R: Monoid](s: S)(f: ((A, I)) => R): R = overF[Const[R, *]](Const[R, B] _ compose f)(s).getConst

  /** synonym for [[traverse]], flipped */
  final def overF[F[_]: Applicative](f: ((A, I)) => F[B])(s: S): F[T] = traverse(s)(f)

  /** modify each focus of a Traversal using a Functor, resulting in a change of type to the full structure */
  def traverse[F[_]: Applicative](s: S)(f: ((A, I)) => F[B]): F[T]
}
