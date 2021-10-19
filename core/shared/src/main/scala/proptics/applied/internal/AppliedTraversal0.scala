package proptics.applied.internal

import cats.Applicative

import proptics.internal.Traversal0

private[proptics] trait AppliedTraversal0[S, T, A, B] extends AppliedSetter0[S, T, A, B] {
  val value: S
  val optic: Traversal0[S, T, A, B]

  /** synonym for [[traverse]] */
  final def overF[F[_]: Applicative](f: A => F[B]): F[T] = optic.overF(f)(value)

  /** modify each focus of a Traversal using a Functor, resulting in a change of type to the full structure */
  def traverse[F[_]: Applicative](f: A => F[B]): F[T] = optic.traverse(value)(f)
}
