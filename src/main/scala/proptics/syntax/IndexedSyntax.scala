package proptics.syntax

import proptics.{IndexedOptic, Optic}
import proptics.internal.{Indexed, Wander}

object IndexedSyntax {
  implicit class IndexedOps[P[_, _], I, S, T, A, B](val traversal: IndexedOptic[P, I, S, T, A, B]) extends AnyVal {
    def unIndex(implicit ev: Wander[P]): Optic[P, S, T, A, B] =
      Optic(pab => traversal(Indexed(ev.dimap[A, B, (I, A), B](pab)(_._2)(identity))))
  }

  implicit class IndexedTraversalOps[P[_, _], I, S, T, A](val indexedTraversal: IndexedOptic[P, I, S, T, A, A]) extends AnyVal {
    def elementsOf(f: I => Boolean)(implicit ev: Wander[P]): IndexedOptic[P, I, S, T, A, A] = ???
  }
}
