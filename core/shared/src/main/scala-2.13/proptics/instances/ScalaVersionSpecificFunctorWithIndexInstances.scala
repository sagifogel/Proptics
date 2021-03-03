package proptics.instances

import scala.collection.compat.immutable.ArraySeq

import cats.Traverse

import proptics.indices.FunctorWithIndex

private[instances] trait ScalaVersionSpecificFunctorWithIndexInstances {
  implicit val functorWithIndexLazyList: FunctorWithIndex[LazyList, Int] = new FunctorWithIndex[LazyList, Int] {
    override def mapWithIndex[A, B](f: (A, Int) => B)(fa: LazyList[A]): LazyList[B] =
      Traverse[LazyList].mapWithIndex(fa)(f)
  }

  implicit val functorWithIndexArraySeq: FunctorWithIndex[ArraySeq, Int] = new FunctorWithIndex[ArraySeq, Int] {
    override def mapWithIndex[A, B](f: (A, Int) => B)(fa: ArraySeq[A]): ArraySeq[B] =
      Traverse[ArraySeq].mapWithIndex(fa)(f)
  }
}
