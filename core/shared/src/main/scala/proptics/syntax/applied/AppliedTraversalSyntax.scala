package proptics.syntax.applied

import scala.reflect.ClassTag

import cats.data.{NonEmptyList, State}
import cats.{Applicative, Bitraverse, Traverse}

import proptics.applied.{AppliedLens_, AppliedTraversal_}
import proptics.internal.{Bazaar, Sellable}
import proptics.std.list._
import proptics.std.string.{mkString => mkStr, _}
import proptics.syntax.traversal._
import proptics.{AppliedTraversal, Traversal, Traversal_}

trait AppliedTraversalSyntax {
  implicit final def appliedTraversalOpsWithTraverse[F[_], A](s: F[A]): AppliedTraversalOpsWithTraverse[F, A] = AppliedTraversalOpsWithTraverse(s)

  implicit final def appliedTraversalElementOps[S, T, A](appliedTraversal: AppliedTraversal_[S, T, A, A]): AppliedTraversalElementOps[S, T, A] = AppliedTraversalElementOps(
    appliedTraversal)

  implicit final def appliedBitraversalElementOps[G[_, _], A](s: G[A, A]): AppliedBitraversalElementOps[G, A] = AppliedBitraversalElementOps(s)

  implicit final def appliedTraversalWithTraverseFocusElementOps[F[_], S, T, A](
      appliedTraversal: AppliedTraversal_[S, T, F[A], F[A]]): AppliedTraversalWithTraverseFocusElementOps[F, S, T, A] =
    AppliedTraversalWithTraverseFocusElementOps(appliedTraversal)

  implicit final def appliedTraversalListOps[S, A](appliedTraversal: AppliedTraversal[S, List[A]]): AppliedTraversalListOps[S, A] = AppliedTraversalListOps(appliedTraversal)

  implicit final def appliedTraversalStringOps[S](appliedTraversal: AppliedTraversal[S, String]): AppliedTraversalStringOps[S] = AppliedTraversalStringOps(appliedTraversal)

  implicit final def appliedTraversalListOfCharsOps[S](appliedTraversal: AppliedTraversal[S, List[Char]]): AppliedTraversalListOfCharsOps[S] = AppliedTraversalListOfCharsOps(
    appliedTraversal)

  implicit final def appliedTraversalFSequenceOps[F[_], G[_], T, A](appliedTraversal: AppliedTraversal_[F[G[A]], F[A], G[A], A]): AppliedTraversalFSequenceOps[F, G, T, A] =
    AppliedTraversalFSequenceOps(appliedTraversal)
}

final case class AppliedTraversalOpsWithTraverse[F[_], A](private val s: F[A]) extends AnyVal {
  /** create a polymorphic [[proptics.Traversal_]] */
  def traversal_[B](implicit ev: Traverse[F]): AppliedTraversal_[F[A], F[B], A, B] =
    AppliedTraversal_(s, Traversal_.fromTraverse[F, A, B])

  /** create a monomorphic [[proptics.Traversal]] */
  def traversal(implicit ev: Traverse[F]): AppliedTraversal[F[A], A] =
    AppliedTraversal_(s, Traversal.fromTraverse[F, A])
}

final case class AppliedTraversalElementOps[S, T, A](private val appliedTraversal: AppliedTraversal_[S, T, A, A]) extends AnyVal {
  def value: S = appliedTraversal.value
  def optic: Traversal_[S, T, A, A] = appliedTraversal.optic

  /** convert a [[Traversal_]] into a [[proptics.Lens]] over a list of the Traversal's foci */
  def partsOf(implicit ev0: Sellable[* => *, Bazaar[* => *, *, *, Unit, *]]): AppliedLens_[S, T, List[A], List[A]] =
    AppliedLens_(value, Traversal.partsOf(optic))

  /** narrow the focus of a [[Traversal_]] to a single element */
  def single(i: Int): AppliedTraversal_[S, T, A, A] = AppliedTraversal_(value, optic.single(i))

  /** traverse elements of a [[Traversal_]] whose index satisfy a predicate */
  def filterByIndex(predicate: Int => Boolean): AppliedTraversal_[S, T, A, A] =
    AppliedTraversal_(value, optic.filterByIndex(predicate))

  /** select the first n elements of a [[Traversal_]] */
  def take(i: Int): AppliedTraversal_[S, T, A, A] = AppliedTraversal_(value, optic.take(i))

  /** select all elements of a [[Traversal_]] except first n ones */
  def drop(i: Int): AppliedTraversal_[S, T, A, A] = AppliedTraversal_(value, optic.drop(i))

  /** take longest prefix of elements of a [[Traversal_]] that satisfy a predicate */
  def takeWhile(predicate: A => Boolean)(implicit ev0: Applicative[State[Boolean, *]]): AppliedTraversal_[S, T, A, A] =
    AppliedTraversal_(value, optic.takeWhile(predicate))

  /** drop longest prefix of elements of a [[Traversal_]] that satisfy a predicate */
  def dropWhile(predicate: A => Boolean)(implicit ev0: Applicative[State[Boolean, *]]): AppliedTraversal_[S, T, A, A] =
    AppliedTraversal_(value, optic.dropWhile(predicate))

  /** filter out elements that do not match the predicate, of optics composed with this [[Traversal_]] */
  def filter(predicate: A => Boolean): AppliedTraversal_[S, T, A, A] =
    AppliedTraversal_(value, optic.andThen(Traversal.filter[A](predicate)))

  /** filter out elements that match the predicate, of optics composed with this [[Traversal_]] */
  def filterNot(predicate: A => Boolean): AppliedTraversal_[S, T, A, A] =
    filter(a => !predicate(a))
}

final case class AppliedBitraversalElementOps[G[_, _], A](private val s: G[A, A]) extends AnyVal {
  /** create a polymorphic [[proptics.Traversal_]] for structure `G[A, A]` that can traverse both sides */
  def bitraverse_[B](implicit ev: Bitraverse[G]): AppliedTraversal_[G[A, A], G[B, B], A, B] =
    AppliedTraversal_(s, Traversal_.both[G, A, B])

  /** create a monomorphic [[proptics.Traversal_]] for structure `G[A, A]` that can traverse both sides */
  def bitraverse(implicit ev: Bitraverse[G]): AppliedTraversal[G[A, A], A] =
    AppliedTraversal_(s, Traversal.both[G, A])
}

final case class AppliedTraversalWithTraverseFocusElementOps[F[_], S, T, A](private val appliedTraversal: AppliedTraversal_[S, T, F[A], F[A]]) extends AnyVal {
  def value: S = appliedTraversal.value
  def optic: Traversal_[S, T, F[A], F[A]] = appliedTraversal.optic

  /** select the first n elements of a [[Traversal_]] */
  def take(i: Int)(implicit ev: Traverse[F]): AppliedTraversal_[S, T, A, A] =
    AppliedTraversal_(value, optic.andThen(Traversal.take[F, A](i)))

  /** select all elements of a [[Traversal_]] except first n ones */
  def drop(i: Int)(implicit ev: Traverse[F]): AppliedTraversal_[S, T, A, A] =
    AppliedTraversal_(value, optic.andThen(Traversal.drop[F, A](i)))

  /** take longest prefix of elements of a [[Traversal_]] that satisfy a predicate */
  def takeWhile(predicate: A => Boolean)(implicit ev0: Applicative[State[Boolean, *]], ev1: Traverse[F]): AppliedTraversal_[S, T, A, A] =
    AppliedTraversal_(value, optic.andThen(Traversal.takeWhile[F, A](predicate)))

  /** drop longest prefix of elements of a [[Traversal_]] that satisfy a predicate */
  def dropWhile(predicate: A => Boolean)(implicit ev0: Applicative[State[Boolean, *]], ev1: Traverse[F]): AppliedTraversal_[S, T, A, A] =
    AppliedTraversal_(value, optic.andThen(Traversal.dropWhile[F, A](predicate)))

  /** compose this [[Traversal_]] with a [[Traversal_]], having this [[Traversal_]] applied first */
  def andThenTraverse(implicit ev: Traverse[F]): AppliedTraversal_[S, T, A, A] =
    AppliedTraversal_(value, optic.andThen(Traversal.fromTraverse[F, A]))
}

final case class AppliedTraversalListOps[S, A](private val appliedTraversal: AppliedTraversal[S, List[A]]) extends AnyVal {
  /** convert from a [[List[A]]] to a [[Vector[A]] */
  def toVector: AppliedTraversal[S, Vector[A]] = appliedTraversal.andThen(listToVector[A])

  /** convert from a [[List[A]]] to a [[Array[A]] */
  def toArray(implicit ev: ClassTag[A]): AppliedTraversal[S, Array[A]] = appliedTraversal.andThen(listToArray[A])

  /** convert from a [[List[A]]] to a [[NonEmptyList[A]] */
  def toNel: AppliedTraversal[S, NonEmptyList[A]] = appliedTraversal.andThen(listToNonEmptyList[A])
}

final case class AppliedTraversalListOfCharsOps[S](private val appliedTraversal: AppliedTraversal[S, List[Char]]) extends AnyVal {
  /** convert from a [[List[char]]] to a [[String]] */
  def mkString: AppliedTraversal[S, String] = appliedTraversal.andThen(charsToString)
}

final case class AppliedTraversalStringOps[S](private val appliedTraversal: AppliedTraversal[S, String]) extends AnyVal {
  /** convert from a [[List[char]]] to a [[String]] */
  def toChars: AppliedTraversal[S, List[Char]] = appliedTraversal.andThen(stringToChars)

  /** shows all elements of a collection in a string using a separator string */
  def mkString(sep: String): AppliedTraversal_[S, S, String, List[String]] = appliedTraversal.andThen(mkStr(sep))

  /** fold over the individual words of a String */
  def toWords: AppliedTraversal[S, String] = appliedTraversal.andThen(words)

  def takeWords(i: Int): AppliedTraversal[S, String] = appliedTraversal.andThen(words.take(i))
}

final case class AppliedTraversalFSequenceOps[F[_], G[_], T, A](private val appliedTraversal: AppliedTraversal_[F[G[A]], F[A], G[A], A]) extends AnyVal {
  /** invert a structure of `F[G[A]]` into `G[F[A]]` */
  def sequence(implicit ev: Applicative[G]): G[F[A]] = appliedTraversal.optic.traverse(appliedTraversal.value)(identity)
}
