package proptics.syntax.applied

import scala.reflect.ClassTag

import cats.data.NonEmptyList
import cats.{Applicative, Foldable, Traverse}

import proptics._
import proptics.applied.{AppliedFold_, AppliedLens_, AppliedTraversal_}
import proptics.std.list._
import proptics.std.string.{mkString => mkStr, _}
import proptics.std.tuple.{_1P, _2P}
import proptics.syntax.traversal._

trait AppliedLensSyntax {
  implicit final def tuple2ToPolyAppliedLensOps[A, B](s: (A, B)): Tuple2ToPolyAppliedLensOps[A, B] = Tuple2ToPolyAppliedLensOps(s)

  implicit final def appliedPolyLensOps[S, T, F[_], A](appliedLens: AppliedLens_[S, T, F[A], A]): AppliedPolyLensOps[S, T, F, A] = AppliedPolyLensOps(appliedLens)

  implicit final def appliedLensListOps[S, A](appliedLens: AppliedLens[S, List[A]]): AppliedLensListOps[S, A] = AppliedLensListOps(appliedLens)

  implicit final def appliedLensListOfCharOps[S](appliedLens: AppliedLens[S, List[Char]]): AppliedLensListOfCharsOps[S] = AppliedLensListOfCharsOps(appliedLens)

  implicit final def appliedLensStringsOps[S, A](appliedLens: AppliedLens[S, String]): AppliedLensStringsOps[S] = AppliedLensStringsOps(appliedLens)
}

final case class Tuple2ToPolyAppliedLensOps[A, B](private val s: (A, B)) extends AnyVal {
  /** select the first element of a tuple using polymorphic [[proptics.Lens_]] */
  def first_[C]: AppliedLens_[(A, B), (C, B), A, C] = AppliedLens_(s, _1P[A, C, B])

  /** select the second element of a tuple using polymorphic [[proptics.Lens_]] */
  def second_[C]: AppliedLens_[(A, B), (A, C), B, C] = AppliedLens_(s, _2P[B, C, A])
}

final case class AppliedLensTraversalOps[F[_], S, T, A](private val appliedLens: AppliedLens_[S, T, F[A], F[A]]) extends AnyVal {
  def value: S = appliedLens.value
  def optic: Lens_[S, T, F[A], F[A]] = appliedLens.optic

  /** compose this [[AppliedLens_]] with a [[proptics.Fold]] having this [[AppliedLens_]] applied first */
  def andThenFold(implicit ev: Foldable[F]): AppliedFold_[S, T, A, A] =
    AppliedFold_(appliedLens.value, appliedLens.optic.andThen(Fold.fromFoldable[F, A]))

  /** compose this [[AppliedLens_]] with a [[Traversal]], having this [[AppliedLens_]] applied first */
  def andThenTraverse(implicit ev: Traverse[F]): AppliedTraversal_[S, T, A, A] =
    AppliedTraversal_(value, optic.andThen(Traversal.fromTraverse[F, A]))
}

final case class AppliedPolyLensOps[S, T, F[_], A](private val appliedLens: AppliedLens_[S, T, F[A], A]) extends AnyVal {
  /** invert a structure of S containing F[A] to F[T], a structure T containing A's inside an Applicative Functor */
  def sequence(implicit ev: Applicative[F]): F[T] =
    appliedLens.optic.traverse[F](appliedLens.value)(identity)
}

final case class AppliedLensListOps[S, A](private val appliedLens: AppliedLens[S, List[A]]) extends AnyVal {
  /** convert from a [[List[A]]] to a [[Vector[A]] */
  def toVector: AppliedLens[S, Vector[A]] = appliedLens.andThen(listToVector[A])

  /** convert from a [[List[A]]] to a [[Array[A]] */
  def toArray(implicit ev: ClassTag[A]): AppliedLens[S, Array[A]] = appliedLens.andThen(listToArray[A])

  /** convert from a [[List[A]]] to a [[NonEmptyList[A]] */
  def toNel: AppliedAffineTraversal[S, NonEmptyList[A]] = appliedLens.andThen(listToNonEmptyList[A])
}

final case class AppliedLensListOfCharsOps[S](private val appliedLens: AppliedLens[S, List[Char]]) extends AnyVal {
  /** convert from a [[List[char]]] to a [[String]] */
  def mkString: AppliedLens[S, String] = appliedLens.andThen(charsToString)
}

final case class AppliedLensStringsOps[S](private val appliedLens: AppliedLens[S, String]) extends AnyVal {
  /** convert from a [[List[char]]] to a [[String]] */
  def toChars: AppliedLens[S, List[Char]] = appliedLens.andThen(stringToChars)

  /** shows all elements of a collection in a string using a separator string */
  def mkString(sep: String): AppliedLens_[S, S, String, List[String]] = appliedLens.andThen(mkStr(sep))

  /** fold over the individual words of a String */
  def toWords: AppliedTraversal[S, String] = appliedLens.andThen(words)

  def takeWords(i: Int): AppliedTraversal[S, String] = appliedLens.andThen(words.take(i))
}
