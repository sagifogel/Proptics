package proptics.syntax

import cats.{Eval, Monoid}

import proptics.indices.FoldableWithIndex

trait FoldableWithIndexSyntax {
  implicit final def foldableWithIndexOps[F[_], A](fa: F[A]): FoldableWithIndexOps[F, A] = FoldableWithIndexOps[F, A](fa)
}

final case class FoldableWithIndexOps[F[_], A](private val fa: F[A]) extends AnyVal {
  def foldMap[B](f: A => B)(implicit B: Monoid[B], ev: FoldableWithIndex[F, _]): B =
    ev.foldMap(fa)(f)

  def foldLeft[B](b: B)(f: (B, A) => B)(implicit ev: FoldableWithIndex[F, _]): B =
    ev.foldLeft(fa, b)(f)

  def foldRight[B](lb: Eval[B])(f: (A, Eval[B]) => Eval[B])(implicit ev: FoldableWithIndex[F, _]): Eval[B] =
    ev.foldRight(fa, lb)(f)

  def isEmpty[B](implicit ev: FoldableWithIndex[F, _]): Boolean =
    ev.isEmpty(fa)

  def foldMapWithIndex[I, B: Monoid](f: (A, I) => B)(implicit ev: FoldableWithIndex[F, I]): B =
    ev.foldMapWithIndex(f)(fa)

  def foldLeftWithIndex[I, B](f: (B, (A, I)) => B)(b: B)(implicit ev: FoldableWithIndex[F, I]): B =
    ev.foldLeftWithIndex(f)(fa, b)

  def foldRightWithIndex[I, B](f: ((A, I), Eval[B]) => Eval[B])(lb: Eval[B])(implicit ev: FoldableWithIndex[F, I]): Eval[B] =
    ev.foldRightWithIndex(f)(fa, lb)
}
