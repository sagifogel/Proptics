package proptics.std

import scala.reflect.ClassTag

import proptics.Iso

trait ListOptics {
  final def listToArray[A: ClassTag]: Iso[List[A], Array[A]] = Iso.iso[List[A], Array[A]](_.toArray)(_.toList)

  final def listToVector[A]: Iso[List[A], Vector[A]] = Iso.iso[List[A], Vector[A]](_.toVector)(_.toList)

  final val charsToString: Iso[List[Char], String] = Iso.iso[List[Char], String](_.mkString)(_.toList)
}
