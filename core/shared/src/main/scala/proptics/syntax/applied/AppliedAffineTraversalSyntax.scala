package proptics.syntax.applied

import scala.reflect.ClassTag

import cats.data.NonEmptyList
import cats.{Foldable, Traverse}

import proptics.applied.{AppliedAffineTraversal_, AppliedFold_, AppliedTraversal_}
import proptics.std.list._
import proptics.std.string.{mkString => mkStr, _}
import proptics.{AffineTraversal_, AppliedAffineTraversal, AppliedFold, Fold, Traversal}

trait AppliedAffineTraversalSyntax {
  implicit final def appliedAffineTraversalOps[F[_], S, T, A](appliedAffineTraversal: AppliedAffineTraversal_[S, T, F[A], F[A]]): AppliedAffineTraversalOps[F, S, T, A] =
    AppliedAffineTraversalOps(appliedAffineTraversal)

  implicit final def appliedAffineTraversalListOps[S, A](appliedAffineTraversal: AppliedAffineTraversal[S, List[A]]): AppliedAffineTraversalListOps[S, A] =
    AppliedAffineTraversalListOps(appliedAffineTraversal)

  implicit final def appliedAffineTraversalListOfCharsOps[S](appliedAffineTraversal: AppliedAffineTraversal[S, List[Char]]): AppliedAffineTraversalListOfCharsOps[S] =
    AppliedAffineTraversalListOfCharsOps(appliedAffineTraversal)

  implicit final def appliedAffineTraversalStringOps[S](appliedAffineTraversal: AppliedAffineTraversal[S, String]): AppliedAffineTraversalStringOps[S] =
    AppliedAffineTraversalStringOps(appliedAffineTraversal)
}

final case class AppliedAffineTraversalOps[F[_], S, T, A](private val appliedAffineTraversal: AppliedAffineTraversal_[S, T, F[A], F[A]]) extends AnyVal {
  def value: S = appliedAffineTraversal.value
  def optic: AffineTraversal_[S, T, F[A], F[A]] = appliedAffineTraversal.optic

  /** compose this [[AffineTraversal_]] with a [[Traversal]], having this [[AffineTraversal_]] applied first */
  def andThenTraverse(implicit ev: Traverse[F]): AppliedTraversal_[S, T, A, A] =
    AppliedTraversal_(value, optic.andThen(Traversal.fromTraverse[F, A]))

  /** compose this [[AffineTraversal_]] with a [[Fold]] having this [[AffineTraversal_]] applied first */
  def andThenFold(implicit ev: Foldable[F]): AppliedFold_[S, T, A, A] =
    AppliedFold_(appliedAffineTraversal.value, appliedAffineTraversal.optic.andThen(Fold.fromFoldable[F, A]))
}

final case class AppliedAffineTraversalListOps[S, A](private val appliedAffineTraversal: AppliedAffineTraversal[S, List[A]]) extends AnyVal {
  /** convert from a [[List[A]]] to a [[Vector[A]] */
  def toVector: AppliedAffineTraversal[S, Vector[A]] = appliedAffineTraversal.andThen(listToVector[A])

  /** convert from a [[List[A]]] to a [[Array[A]] */
  def toArray(implicit ev: ClassTag[A]): AppliedAffineTraversal[S, Array[A]] = appliedAffineTraversal.andThen(listToArray[A])

  /** convert from a [[List[A]]] to a [[NonEmptyList[A]] */
  def toNel: AppliedAffineTraversal[S, NonEmptyList[A]] = appliedAffineTraversal.andThen(listToNonEmptyList[A])
}

final case class AppliedAffineTraversalStringOps[S](private val appliedAffineTraversal: AppliedAffineTraversal[S, String]) extends AnyVal {
  /** convert from a [[List[char]]] to a [[String]] */
  def toChars: AppliedAffineTraversal[S, List[Char]] = appliedAffineTraversal.andThen(stringToChars)

  /** shows all elements of a collection in a string using a separator string */
  def mkString(sep: String): AppliedAffineTraversal_[S, S, String, List[String]] = appliedAffineTraversal.andThen(mkStr(sep))

  /** fold over the individual words of a String */
  def toWords: AppliedFold[S, String] = appliedAffineTraversal.andThen(words)
}

final case class AppliedAffineTraversalListOfCharsOps[S](private val appliedAffineTraversal: AppliedAffineTraversal[S, List[Char]]) extends AnyVal {
  /** convert from a [[List[char]]] to a [[String]] */
  def mkString: AppliedAffineTraversal[S, String] = appliedAffineTraversal.andThen(charsToString)
}
