package proptics.syntax

import proptics.{IndexedTraversal, Traversal}

object AsTraversalSyntax {
  implicit class IsoAsLensOps[I, S, A](val indexedTraversal: IndexedTraversal[I, S, A]) extends AnyVal {
    def asTraversal: Traversal[S, A] = indexedTraversal.asTraversal_
  }
}
