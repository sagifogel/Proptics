package proptics.syntax

import cats.instances.int._
import cats.syntax.eq._
import proptics.{IndexedTraversal, Optic}
import proptics.internal.Wander
import proptics.syntax.IndexedSyntax._

object TraverseSyntax {
  implicit class TraversalOps[P[_, _], S, T, A, B](val traversal: Optic[P, S, T, A, B]) extends AnyVal {
    def positions(implicit ev: Wander[P]): IndexedTraversal[P, Int, S, T, A, B] = ???
  }

  implicit class TraversalElementsOps[P[_, _]: Wander, S, T, A](val traversal: Optic[P, S, T, A, A]) {
    def element(i: Int): Optic[P, S, T, A, A] = traversal.positions.elementsOf(_ === 0).unIndex
  }
}
