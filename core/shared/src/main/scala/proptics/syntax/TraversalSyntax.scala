package proptics.syntax

import cats.Applicative
import cats.syntax.eq._

import proptics.Traversal_
import proptics.syntax.indexedTraversal._

trait TraversalSyntax {
  implicit final def traversalElementOps[S, T, A](traversal: Traversal_[S, T, A, A]): TraversalElementOps[S, T, A] = TraversalElementOps(traversal)

  implicit final def traversalSequenceOps[F[_], S, T, A](traversal: Traversal_[S, T, F[A], A]): TraversalSequenceOps[F, S, T, A] = TraversalSequenceOps(traversal)
}

final case class TraversalElementOps[S, T, A](private val traversal: Traversal_[S, T, A, A]) extends AnyVal {
  /** combine an index and an [[Traversal_]] to narrow the focus to a single element */
  def element(i: Int): Traversal_[S, T, A, A] =
    traversal.asIndexableTraversal.filterByIndex(_ === i).unIndex

  /** traverse elements of an [[Traversal_]] whose index satisfy a predicate */
  def filterByIndex(predicate: Int => Boolean): Traversal_[S, T, A, A] =
    traversal.asIndexableTraversal.filterByIndex(predicate).unIndex
}

final case class TraversalSequenceOps[F[_], S, T, A](private val traversal: Traversal_[S, T, F[A], A]) extends AnyVal {
  /** invert a structure of S containing F[A] to F[T], a structure T containing A's inside an Applicative Functor */
  def sequence(s: S)(implicit ev: Applicative[F]): F[T] = traversal.traverse(s)(identity)
}
