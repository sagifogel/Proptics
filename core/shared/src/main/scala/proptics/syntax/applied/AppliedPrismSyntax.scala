package proptics.syntax.applied

import scala.reflect.ClassTag

import cats.data.NonEmptyList

import proptics.AppliedPrism
import proptics.applied.AppliedPrism_
import proptics.std.list._
import proptics.std.string.{mkString => mkStr, _}

trait AppliedPrismSyntax {
  implicit final def appliedPrismListOps[S, A](appliedPrism: AppliedPrism[S, List[A]]): AppliedPrismListOps[S, A] = AppliedPrismListOps(appliedPrism)

  implicit final def appliedPrismListOfCharsOps[S](appliedPrism: AppliedPrism[S, List[Char]]): AppliedPrismListOfCharsOps[S] = AppliedPrismListOfCharsOps(appliedPrism)

  implicit final def appliedPrismStringOps[S](appliedPrism: AppliedPrism[S, String]): AppliedPrismStringOps[S] = AppliedPrismStringOps(appliedPrism)
}

final case class AppliedPrismListOps[S, A](private val appliedPrism: AppliedPrism[S, List[A]]) extends AnyVal {
  /** convert from a [[List[A]]] to a [[Vector[A]] */
  def toVector: AppliedPrism[S, Vector[A]] = appliedPrism.andThen(listToVector[A])

  /** convert from a [[List[A]]] to a [[Array[A]] */
  def toArray(implicit ev: ClassTag[A]): AppliedPrism[S, Array[A]] = appliedPrism.andThen(listToArray[A])

  /** convert from a [[List[A]]] to a [[NonEmptyList[A]] */
  def toNel: AppliedPrism[S, NonEmptyList[A]] = appliedPrism.andThen(listToNonEmptyList[A])
}

final case class AppliedPrismStringOps[S](private val appliedPrism: AppliedPrism[S, String]) extends AnyVal {
  /** convert from a [[List[char]]] to a [[String]] */
  def toChars: AppliedPrism[S, List[Char]] = appliedPrism.andThen(stringToChars)

  /** shows all elements of a collection in a string using a separator string */
  def mkString(sep: String): AppliedPrism_[S, S, String, List[String]] = appliedPrism.andThen(mkStr(sep))
}

final case class AppliedPrismListOfCharsOps[S](private val appliedPrism: AppliedPrism[S, List[Char]]) extends AnyVal {
  /** convert from a [[List[char]]] to a [[String]] */
  def mkString: AppliedPrism[S, String] = appliedPrism.andThen(charsToString)
}
