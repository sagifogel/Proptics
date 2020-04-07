package proptics.syntax

import cats.data.State
import cats.instances.int._
import cats.syntax.eq._
import proptics.Traversal_
import proptics.internal.Wander
import proptics.syntax.IndexedTraversalSyntax._

object TraversalSyntax {
  implicit class TraversalElementsOps[P[_, _], I, S, T, A](val traversal: Traversal_[S, T, A, A]) extends AnyVal {
    def element(i: Int)(implicit ev0: State[Int, A], ev1: Wander[P]): Traversal_[S, T, A, A] =
      traversal.positions.elements(_ === 0).unIndex
  }
}
