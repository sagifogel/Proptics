package proptics.instances

import scala.collection.immutable.ArraySeq

import proptics.Traversal
import proptics.typeclass.Each

private[instances] trait ScalaVersionSpecificEachInstances {
  implicit final def eachLazyList[A]: Each[LazyList[A], A] = new Each[LazyList[A], A] {
    override def each: Traversal[LazyList[A], A] = Traversal.fromTraverse[LazyList, A]
  }

  implicit final def eachArraySeq[A]: Each[ArraySeq[A], A] = new Each[ArraySeq[A], A] {
    override def each: Traversal[ArraySeq[A], A] = Traversal.fromTraverse[ArraySeq, A]
  }
}
