package proptics.syntax

import proptics.{IndexedTraversal_, Traversal}

object AsTraversalSyntax {
  implicit class IsoAsLensOps[I, S, A](val indexedTraversal: IndexedTraversal_[I, S, A]) extends AnyVal {
    def asTraversal: Traversal[S, A] = indexedTraversal.asTraversal_
  }
}
