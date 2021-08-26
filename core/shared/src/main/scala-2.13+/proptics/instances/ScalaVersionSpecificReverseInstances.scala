package proptics.instances

import scala.collection.immutable.ArraySeq
import scala.reflect.ClassTag

import proptics.Iso
import proptics.typeclass.Reverse

private[instances] trait ScalaVersionSpecificReverseInstances {
  implicit final def reverseLazyList[A]: Reverse[LazyList[A], LazyList[A]] = new Reverse[LazyList[A], LazyList[A]] {
    override def reverse: Iso[LazyList[A], LazyList[A]] = Iso.involuted[LazyList[A]](_.reverse)
  }
}
