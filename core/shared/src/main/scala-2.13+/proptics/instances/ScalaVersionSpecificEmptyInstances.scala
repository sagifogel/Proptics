package proptics.instances

import scala.collection.immutable.ArraySeq
import scala.reflect.ClassTag

import proptics.Prism
import proptics.typeclass.Empty

private[instances] trait ScalaVersionSpecificEmptyInstances {
  implicit final def emptyLazyList[A]: Empty[LazyList[A]] = new Empty[LazyList[A]] {
    override def empty: Prism[LazyList[A], Unit] = Prism.nearly(LazyList.empty[A])(_.isEmpty)
  }

  implicit final def emptyArraySeq[A: ClassTag]: Empty[ArraySeq[A]] = new Empty[ArraySeq[A]] {
    override def empty: Prism[ArraySeq[A], Unit] = Prism.nearly(ArraySeq.untagged.empty[A])(_.isEmpty)
  }
}
