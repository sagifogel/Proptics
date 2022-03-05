package proptics.instances

import scala.collection.compat.immutable.ArraySeq

import cats.Eval

import proptics.indices.FoldableWithIndex
import proptics.instances.FoldableWithIndexInstances._

private[instances] trait ScalaVersionSpecificFoldableWithIndexInstances {
  implicit final val foldableWithIndexStream: FoldableWithIndex[Stream, Int] = new FoldableWithIndex[Stream, Int] {
    override def foldLeftWithIndex[A, B](f: (B, (A, Int)) => B)(fa: Stream[A], b: B): B =
      listLikeFoldLeftWithIndex(fa.foldLeft[(B, Int)])(f)(b)

    override def foldRightWithIndex[A, B](f: ((A, Int), Eval[B]) => Eval[B])(fa: Stream[A], lb: Eval[B]): Eval[B] =
      listLikeFoldRightWithIndex(f)(fa.iterator, lb)

    override def exists[A](fa: Stream[A])(f: A => Boolean): Boolean = fa.exists(f)
  }

  implicit final val foldableWithIndexArraySeq: FoldableWithIndex[ArraySeq, Int] = new FoldableWithIndex[ArraySeq, Int] {
    override def foldLeftWithIndex[A, B](f: (B, (A, Int)) => B)(fa: ArraySeq[A], b: B): B =
      listLikeFoldLeftWithIndex(fa.foldLeft[(B, Int)])(f)(b)

    override def foldRightWithIndex[A, B](f: ((A, Int), Eval[B]) => Eval[B])(fa: ArraySeq[A], lb: Eval[B]): Eval[B] =
      listLikeFoldRightWithIndex(f)(fa.iterator, lb)

    override def exists[A](fa: ArraySeq[A])(f: A => Boolean): Boolean = fa.exists(f)
  }
}
