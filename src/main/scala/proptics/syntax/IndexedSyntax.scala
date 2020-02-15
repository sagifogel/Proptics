package proptics.syntax

import proptics.{IndexedOptic, IndexedTraversal}

object IndexedSyntax {
  implicit class IndexedTraversalOps[P[_, _], I, S, T, A](val indexedTraversal: IndexedTraversal[I, S, T, A, A]) extends AnyVal {
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