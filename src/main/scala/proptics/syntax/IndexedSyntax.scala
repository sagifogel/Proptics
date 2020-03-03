package proptics.syntax

import cats.Applicative
import proptics.internal.{Indexed, Wander}
import proptics.profunctor.Star
import proptics.rank2types.LensLikeIndexedTraversal
import proptics.{IndexedOptic, IndexedTraversal}
import proptics.IndexedTraversal.wander

object IndexedSyntax {
  implicit class IndexedTraversalOps[P[_, _], I, S, T, A](val indexedTraversal: IndexedTraversal[I, S, T, A, A]) extends AnyVal {
    def elementsOf(pr: I => Boolean)(implicit ev0: Wander[P]): IndexedOptic[P, I, S, T, A, A] = {
      val indexTraversal: IndexedTraversal[I, S, T, A, A] =
        wander(new LensLikeIndexedTraversal[I, S, T, A, A] {
          override def apply[F[_]](f: I => A => F[A])(implicit ev: Applicative[F]): S => F[T] = {
            val starIndex: Indexed[Star[F, *, *], I, A, A] = Indexed[Star[F, *, *], I, A, A](Star {
              case (i, a) => if (pr(i)) f(i)(a) else ev.pure(a)
            })

            indexedTraversal(starIndex).runStar
          }
        })

      IndexedOptic(indexTraversal(_))
    }
  }
}