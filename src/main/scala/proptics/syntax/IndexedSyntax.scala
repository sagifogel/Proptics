package proptics.syntax

import proptics.internal.{Indexed, Wander}
import proptics.{IndexedOptic, IndexedTraversal, Optic}

object IndexedSyntax {
  implicit class IndexedOps[P[_, _], I, S, T, A, B](val traversal: IndexedOptic[P, I, S, T, A, B]) extends AnyVal {
    def unIndex(implicit ev: Wander[P]): Optic[P, S, T, A, B] =
      Optic(pab => traversal(Indexed(ev.dimap[A, B, (I, A), B](pab)(_._2)(identity))))
  }

  implicit class IndexedTraversalOps[P[_, _], I, S, T, A](val indexedTraversal: IndexedTraversal[P, I, S, T, A, A]) extends AnyVal {
    def elementsOf(pr: I => Boolean): IndexedOptic[P, I, S, T, A, A] = ???
//      IndexedTraversal.iwander(new LensLikeIndexedTraversal[I, S, T, A, A] {
//        override def apply[F[_]](f: I => A => F[A])(implicit ev1: Applicative[F]): S => F[T] = s => {
//          val starIndex: Indexed[Star[F, *, *], I, A, A] = Indexed[Star[F, *, *], I, A, A](Star {
//            case (i, a) => if (pr(i)) f(i)(a) else ev1.pure(a)
//          })
//
//
//          indexedTraversal.(starIndex)
//        }
//      }
  }
}