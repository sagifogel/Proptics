package proptics.syntax

import cats.instances.int._
import cats.syntax.eq._
import proptics.internal.Wander
import proptics.syntax.IndexedSyntax._
import proptics.{IndexedTraversal, Optic, Traversal}

object TraversalSyntax {
  implicit class TraversalOps[P[_, _], I, S, T, A, B](val traversal: Traversal[S, T, A, B]) extends AnyVal {
    def positions(implicit ev: Wander[P]): IndexedTraversal[Int, S, T, A, B] = ???
  }

  implicit class TraversalElementsOps[P[_, _]: Wander, I, S, T, A](val traversal: Traversal[S, T, A, A]) {
    def element(i: Int): Optic[P, S, T, A, A] = traversal.positions.elementsOf(_ === 0).unIndex
  }
}
