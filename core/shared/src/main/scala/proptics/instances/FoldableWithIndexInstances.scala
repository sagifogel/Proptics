package proptics.instances

import cats.Eval
import cats.data.NonEmptyList

import proptics.indices.FoldableWithIndex

trait FoldableWithIndexInstances {
  implicit val foldableWithIndexList: FoldableWithIndex[List, Int] = new FoldableWithIndex[List, Int] {
    override def foldLeftWithIndex[A, B](f: (B, (A, Int)) => B)(fa: List[A], b: B): B =
      fa.foldLeft((b, 0)) { case ((b, i), a) => (f(b, (a, i)), i + 1) }._1

    override def foldRightWithIndex[A, B](f: ((A, Int), Eval[B]) => Eval[B])(fa: List[A], lb: Eval[B]): Eval[B] =
      fa.foldRight(lb.map((_, 0))) { (a, eval) =>
        eval.flatMap { case (b, i) => f((a, i), Eval.now(b)).map((_, i + 1)) }
      }.map(_._1)
  }

  implicit val foldableWithIndexNel: FoldableWithIndex[NonEmptyList, Int] = new FoldableWithIndex[NonEmptyList, Int] {
    override def foldLeftWithIndex[A, B](f: (B, (A, Int)) => B)(fa: NonEmptyList[A], b: B): B =
      fa.foldLeft((b, 0)) { case ((b, i), a) => (f(b, (a, i)), i + 1) }._1

    override def foldRightWithIndex[A, B](f: ((A, Int), Eval[B]) => Eval[B])(fa: NonEmptyList[A], lb: Eval[B]): Eval[B] =
      fa.foldRight(lb.map((_, 0))) { (a, eval) =>
        eval.flatMap { case (b, i) => f((a, i), Eval.now(b)).map((_, i + 1)) }
      }.map(_._1)
  }

  implicit def foldableWithIndexMap[K]: FoldableWithIndex[Map[K, *], K] = new FoldableWithIndex[Map[K, *], K] {
    override def foldLeftWithIndex[A, B](f: (B, (A, K)) => B)(fa: Map[K, A], b: B): B =
      fa.foldLeft(b) { case (b, pair) => f(b, pair.swap) }

    override def foldRightWithIndex[A, B](f: ((A, K), Eval[B]) => Eval[B])(fa: Map[K, A], lb: Eval[B]): Eval[B] =
      fa.foldRight(lb) { case (pair, b) => f(pair.swap, lb) }
  }
}
