package proptics.instances

import scala.collection.immutable.ArraySeq

import cats.instances.arraySeq._
import cats.instances.lazyList._
import cats.{Applicative, Eval}

import proptics.indices.TraverseWithIndex
import proptics.instances.foldableWithIndex._
import proptics.instances.functorWithIndex._

private[instances] trait ScalaVersionSpecificTraverseWithIndexInstances {
  implicit final val traverseWithIndexLazyList: TraverseWithIndex[LazyList, Int] = new TraverseWithIndex[LazyList, Int] {
    override def mapWithIndex[A, B](f: (A, Int) => B)(fa: LazyList[A]): LazyList[B] =
      functorWithIndexLazyList.mapWithIndex(f)(fa)

    override def foldLeftWithIndex[A, B](f: (B, (A, Int)) => B)(fa: LazyList[A], b: B): B =
      foldableWithIndexLazyList.foldLeftWithIndex(f)(fa, b)

    override def foldRightWithIndex[A, B](f: ((A, Int), Eval[B]) => Eval[B])(fa: LazyList[A], lb: Eval[B]): Eval[B] =
      foldableWithIndexLazyList.foldRightWithIndex(f)(fa, lb)

    override def traverse[G[_], A, B](fa: LazyList[A])(f: A => G[B])(implicit ev: Applicative[G]): G[LazyList[B]] =
      catsStdInstancesForLazyList.traverse(fa)(f)

    override def exists[A](fa: LazyList[A])(f: A => Boolean): Boolean = foldableWithIndexLazyList.exists(fa)(f)
  }

  implicit final val traverseWithIndexArraySeq: TraverseWithIndex[ArraySeq, Int] = new TraverseWithIndex[ArraySeq, Int] {
    override def mapWithIndex[A, B](f: (A, Int) => B)(fa: ArraySeq[A]): ArraySeq[B] =
      functorWithIndexArraySeq.mapWithIndex(f)(fa)

    override def foldLeftWithIndex[A, B](f: (B, (A, Int)) => B)(fa: ArraySeq[A], b: B): B =
      foldableWithIndexArraySeq.foldLeftWithIndex(f)(fa, b)

    override def foldRightWithIndex[A, B](f: ((A, Int), Eval[B]) => Eval[B])(fa: ArraySeq[A], lb: Eval[B]): Eval[B] =
      foldableWithIndexArraySeq.foldRightWithIndex(f)(fa, lb)

    override def traverse[G[_], A, B](fa: ArraySeq[A])(f: A => G[B])(implicit ev: Applicative[G]): G[ArraySeq[B]] =
      catsStdInstancesForArraySeq.traverse(fa)(f)

    override def exists[A](fa: ArraySeq[A])(f: A => Boolean): Boolean = foldableWithIndexArraySeq.exists(fa)(f)
  }
}
