package proptics.syntax

import cats.{Eval, Monoid}

import proptics.indices.FoldableWithIndex

trait FoldableWithIndexSyntax {
  implicit final def foldableWithIndexOps[F[_], A](fa: F[A]): FoldableWithIndexOps[F, A] = FoldableWithIndexOps[F, A](fa)
}

final case class FoldableWithIndexOps[F[_], A](private val fa: F[A]) extends AnyVal {
  def foldMapWithIndex[I, B: Monoid](f: (A, I) => B)(implicit ev: FoldableWithIndex[F, I]): B =
    ev.foldMapWithIndex(f)(fa)

  def foldLeftWithIndex[I, B](f: (B, (A, I)) => B)(b: B)(implicit ev: FoldableWithIndex[F, I]): B =
    ev.foldLeftWithIndex(f)(fa, b)

  def foldRightWithIndex[I, B](f: ((A, I), Eval[B]) => Eval[B])(lb: Eval[B])(implicit ev: FoldableWithIndex[F, I]): Eval[B] =
    ev.foldRightWithIndex(f)(fa, lb)
}
