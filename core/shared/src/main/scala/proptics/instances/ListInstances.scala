package proptics.instances

import proptics.Iso

import scala.reflect.ClassTag

trait ListInstances {
  implicit final def listToArray[A: ClassTag]: Iso[List[A], Array[A]] = Iso.iso[List[A], Array[A]](_.toArray)(_.toList)

  implicit final def listToVector[A]: Iso[List[A], Vector[A]] = Iso.iso[List[A], Vector[A]](_.toVector)(_.toList)
}
