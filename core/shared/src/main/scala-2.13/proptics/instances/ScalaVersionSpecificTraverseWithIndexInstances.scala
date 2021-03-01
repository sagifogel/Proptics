package proptics.instances

import cats.syntax.traverse._
import cats.{Applicative, Eval}

import proptics.indices.TraverseWithIndex
import proptics.instances.foldableWithIndex._
import proptics.instances.functorWithIndex._

private[instances] trait ScalaVersionSpecificTraverseWithIndexInstances {
  implicit val traverseWithIndexLazyList: TraverseWithIndex[LazyList, Int] = new TraverseWithIndex[LazyList, Int] {
    override def mapWithIndex[A, B](f: (A, Int) => B)(fa: LazyList[A]): LazyList[B] =
      functorWithIndexLazyList.mapWithIndex(f)(fa)

    override def foldLeftWithIndex[A, B](f: (B, (A, Int)) => B)(fa: LazyList[A], b: B): B =
      foldableWithIndexLazyList.foldLeftWithIndex(f)(fa, b)

    override def foldRightWithIndex[A, B](f: ((A, Int), Eval[B]) => Eval[B])(fa: LazyList[A], lb: Eval[B]): Eval[B] =
      foldableWithIndexLazyList.foldRightWithIndex(f)(fa, lb)

    override def traverse[G[_], A, B](fa: LazyList[A])(f: A => G[B])(implicit ev: Applicative[G]): G[LazyList[B]] =
      fa.traverse(f)
  }
}
