package proptics.syntax

import cats.Applicative
import cats.data.State
import cats.syntax.eq._
import proptics.Traversal_
import proptics.profunctor.Wander
import proptics.syntax.indexedTraversal._

trait TraversalSyntax {
  implicit final def traversalElementOps[S, T, A](traversal: Traversal_[S, T, A, A]): TraversalElementOps[S, T, A] = TraversalElementOps(traversal)

  implicit final def traversalSequenceOps[F[_], S, T, A](traversal: Traversal_[S, T, F[A], A]): TraversalSequenceOps[F, S, T, A] = TraversalSequenceOps(traversal)
}

final case class TraversalElementOps[S, T, A](private val traversal: Traversal_[S, T, A, A]) extends AnyVal {
  def element[P[_, _]](i: Int)(implicit ev0: State[Int, A], ev1: Wander[P]): Traversal_[S, T, A, A] =
    traversal.positions.elements(_ === i).unIndex
}

final case class TraversalSequenceOps[F[_], S, T, A](private val traversal: Traversal_[S, T, F[A], A]) extends AnyVal {
  def sequence(s: S)(implicit ev: Applicative[F]): F[T] = traversal.traverse(s)(identity)
}
