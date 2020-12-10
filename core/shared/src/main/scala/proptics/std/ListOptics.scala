package proptics.std

import scala.reflect.ClassTag

import proptics.Iso

trait ListOptics {
  /** a monomorphic [[Iso]] from a list to an array */
  final def listToArray[A: ClassTag]: Iso[List[A], Array[A]] = Iso.iso[List[A], Array[A]](_.toArray)(_.toList)

  /** a monomorphic [[Iso]] from a list to a vector */
  final def listToVector[A]: Iso[List[A], Vector[A]] = Iso.iso[List[A], Vector[A]](_.toVector)(_.toList)

  /** a monomorphic [[Iso]] from a list of chars to a string */
  final val charsToString: Iso[List[Char], String] = Iso.iso[List[Char], String](_.mkString)(_.toList)

  /** a monomorphic [[Iso]] for reversing a list */
  final def reverse[A]: Iso[List[A], List[A]] = Iso.involuted[List[A]](_.reverse)
}
