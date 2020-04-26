package proptics.syntax

import cats.Applicative
import cats.data.State
import cats.instances.int._
import cats.syntax.eq._
import proptics.Traversal_
import proptics.internal.Wander
import proptics.syntax.IndexedTraversalSyntax._

trait TraversalSyntax {
  implicit def traversalElementOps[P[_, _], S, T, A](traversal: Traversal_[S, T, A, A]) = TraversalElementOps(traversal)

  implicit def traversalSequenceOps[F[_], S, T, A](traversal: Traversal_[S, T, F[A], A]) = TraversalSequenceOps(traversal)
}

final case class TraversalElementOps[P[_, _], S, T, A](private val traversal: Traversal_[S, T, A, A]) extends AnyVal {
  def element(i: Int)(implicit ev0: State[Int, A], ev1: Wander[P]): Traversal_[S, T, A, A] =
    traversal.positions.elements(_ === i).unIndex
}

final case class TraversalSequenceOps[F[_], S, T, A](private val traversal: Traversal_[S, T, F[A], A]) extends AnyVal {
  def sequence(s: S)(implicit ev: Applicative[F]): F[T] = traversal.traverse(s)(identity)
}
