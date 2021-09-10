package proptics.instances

import scala.collection.immutable.ArraySeq

import cats.Eval

import proptics.indices.FoldableWithIndex
import proptics.instances.FoldableWithIndexInstances._

private[instances] trait ScalaVersionSpecificFoldableWithIndexInstances {
  implicit final val foldableWithIndexLazyList: FoldableWithIndex[LazyList, Int] = new FoldableWithIndex[LazyList, Int] {
    override def foldLeftWithIndex[A, B](f: (B, (A, Int)) => B)(fa: LazyList[A], b: B): B =
      listLikeFoldLeftWithIndex(fa.foldLeft[(B, Int)])(f)(b)

    override def foldRightWithIndex[A, B](f: ((A, Int), Eval[B]) => Eval[B])(fa: LazyList[A], lb: Eval[B]): Eval[B] =
      listLikeFoldRightWithIndex(f)(fa.iterator, lb)
  }

  implicit final val foldableWithIndexArraySeq: FoldableWithIndex[ArraySeq, Int] = new FoldableWithIndex[ArraySeq, Int] {
    override def foldLeftWithIndex[A, B](f: (B, (A, Int)) => B)(fa: ArraySeq[A], b: B): B =
      listLikeFoldLeftWithIndex(fa.foldLeft[(B, Int)])(f)(b)

    override def foldRightWithIndex[A, B](f: ((A, Int), Eval[B]) => Eval[B])(fa: ArraySeq[A], lb: Eval[B]): Eval[B] =
      listLikeFoldRightWithIndex(f)(fa.iterator, lb)
  }
}
