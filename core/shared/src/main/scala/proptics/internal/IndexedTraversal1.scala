package proptics.internal

import cats.data.Const
import cats.syntax.apply._
import cats.{Applicative, Monoid}

trait IndexedTraversal1[I, S, T, A, B] extends IndexedTraversal0[I, S, T, A, B] with IndexedFold0[I, S, A] {
  /** map each focus of a Traversal to a [[cats.Monoid]], and combine the results */
  final override def foldMap[R: Monoid](s: S)(f: ((A, I)) => R): R = overF[Const[R, *]](Const[R, B] _ compose f)(s).getConst

  /** evaluate each focus and index of an IndexedTraversal from left to right, and ignore the results structure */
  final def sequence_[F[_]](s: S)(implicit ev: Applicative[F]): F[Unit] = traverse_(s)(ev.pure)

  /** map each focus and index of an IndexedTraversal to an effect, from left to right, and ignore the results */
  final def traverse_[F[_], R](s: S)(f: ((A, I)) => F[R])(implicit ev: Applicative[F]): F[Unit] =
    foldLeft[F[Unit]](s)(ev.pure(()))((b, ia) => ev.void(f(ia)) *> b)
}
