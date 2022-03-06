package proptics.applied.internal

import cats.Applicative

import proptics.applied.AppliedFoldCompat
import proptics.internal.Traversal1

private[proptics] trait AppliedTraversal1[S, T, A, B] extends AppliedTraversal0[S, T, A, B] with AppliedFoldCompat[S, A] {
  val value: S
  val optic: Traversal1[S, T, A, B]

  /** evaluate each  focus of a Traversal from left to right, and ignore the results structure */
  final def sequence_[F[_]](implicit ev: Applicative[F]): F[Unit] = optic.sequence_(value)

  /** map each focus of a Traversal to an effect, from left to right, and ignore the results */
  final def traverse_[F[_], R](f: A => F[R])(implicit ev: Applicative[F]): F[Unit] = optic.traverse_(value)(f)
}
