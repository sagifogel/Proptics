package proptics.instances

import proptics.Iso
import proptics.typeclass.Reverse

private[instances] trait ScalaVersionSpecificReverseInstance {
  implicit final def reverseStream[A]: Reverse[Stream[A], Stream[A]] = new Reverse[Stream[A], Stream[A]] {
    override def reverse: Iso[Stream[A], Stream[A]] = Iso.involuted[Stream[A]](_.reverse)
  }
}
