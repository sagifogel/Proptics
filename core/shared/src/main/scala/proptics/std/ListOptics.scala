package proptics.std

import scala.reflect.ClassTag

import cats.data.NonEmptyList

import proptics.{Iso, Prism}

trait ListOptics {
  /** a monomorphic [[Iso]] from a list to an Array */
  final def listToArray[A: ClassTag]: Iso[List[A], Array[A]] = Iso.iso[List[A], Array[A]](_.toArray)(_.toList)

  /** a monomorphic [[Iso]] from a List to a Vector */
  final def listToVector[A]: Iso[List[A], Vector[A]] = Iso.iso[List[A], Vector[A]](_.toVector)(_.toList)

  /** a monomorphic [[Prism]] from a List to a NonEmptyList */
  final def listToNonEmptyList[A]: Prism[List[A], NonEmptyList[A]] =
    Prism.fromPreview[List[A], NonEmptyList[A]](NonEmptyList.fromList)(_.toList)

  /** a monomorphic [[Iso]] from a List[char] to a String */
  final val charsToString: Iso[List[Char], String] = Iso.iso[List[Char], String](_.mkString)(_.toList)
}
