package proptics.syntax

import cats.Applicative
import cats.data.State
import cats.syntax.eq._

import proptics.instances.partsOf._
import proptics.syntax.indexedTraversal._
import proptics.syntax.traversal._
import proptics.{ATraversal, ATraversal_, Lens_}

trait ATraversalSyntax {
  implicit final def aTraversalElementOps[S, T, A](aTraversal: ATraversal_[S, T, A, A]): ATraversalElementOps[S, T, A] = ATraversalElementOps(aTraversal)

  implicit def aTraversalSequenceOps[F[_], I, S, T, A](aTraversal: ATraversal_[S, T, F[A], A]): ATraversalSequenceOps[F, I, S, T, A] = ATraversalSequenceOps(aTraversal)
}

final case class ATraversalElementOps[S, T, A](private val aTraversal: ATraversal_[S, T, A, A]) extends AnyVal {
  /** convert an [[ATraversal]] into a [[Lens]] over a list of the [[ATraversal]]'s foci */
  def partsOf: Lens_[S, T, List[A], List[A]] = ATraversal.partsOf(aTraversal)

  /** narrow the focus of an [[ATraversal_]] to a single element */
  def elementAt(i: Int): ATraversal_[S, T, A, A] = filterByIndex(_ === i)

  /** traverse elements of an [[ATraversal_]] whose index satisfy a predicate */
  def filterByIndex(predicate: Int => Boolean): ATraversal_[S, T, A, A] =
    aTraversal.asTraversal.asIndexableTraversal
      .filterByIndex(predicate)
      .unIndex
      .asATraversal

  /** select the first n elements of an [[ATraversal_]] */
  def take(i: Int): ATraversal_[S, T, A, A] = filterByIndex(_ < i)

  /** select all elements of an [[ATraversal_]] except first n ones */
  def drop(i: Int): ATraversal_[S, T, A, A] = filterByIndex(_ >= i)

  /** take longest prefix of elements of an [[ATraversal_]] that satisfy a predicate */
  def takeWhile(predicate: A => Boolean)(implicit ev0: Applicative[State[Boolean, *]]): ATraversal_[S, T, A, A] =
    aTraversal.asTraversal.takeWhile(predicate).asATraversal

  /** drop longest prefix of elements of an [[ATraversal_]] that satisfy a predicate */
  def dropWhile(predicate: A => Boolean)(implicit ev0: Applicative[State[Boolean, *]]): ATraversal_[S, T, A, A] =
    aTraversal.asTraversal.dropWhile(predicate).asATraversal
}

final case class ATraversalSequenceOps[F[_], I, S, T, A](private val grate: ATraversal_[S, T, F[A], A]) extends AnyVal {
  /** invert a structure of S containing F[A] to F[T], a structure T containing A's inside an Applicative Functor */
  def sequence(s: S)(implicit ev0: Applicative[F]): F[T] = grate.traverse(s)(identity)
}
