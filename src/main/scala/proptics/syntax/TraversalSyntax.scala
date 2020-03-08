package proptics.syntax

import cats.data.State
import cats.instances.int._
import cats.syntax.eq._
import proptics.Traversal
import proptics.internal.Wander
import proptics.syntax.IndexedTraversalSyntax._

object TraversalSyntax {
  implicit class TraversalElementsOps[P[_, _]: Wander, I, S, T, A](val traversal: Traversal[S, T, A, A]) {
    def element(i: Int)(implicit ev: State[Int, A]): Traversal[S, T, A, A] =
      traversal.positions.elements(_ === 0).unIndex
  }
}
