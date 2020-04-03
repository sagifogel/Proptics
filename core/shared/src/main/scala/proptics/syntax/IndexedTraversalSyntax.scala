package proptics.syntax

import cats.Applicative
import proptics.IndexedTraversal_
import proptics.IndexedTraversal_.wander
import proptics.internal.{Indexed, Wander}
import proptics.profunctor.Star
import proptics.rank2types.LensLikeIndexedTraversal

object IndexedTraversalSyntax {
  implicit class IndexedTraversalOps[P[_, _], I, S, T, A](val indexedTraversal: IndexedTraversal_[I, S, T, A, A]) extends AnyVal {
    def elements(pr: I => Boolean)(implicit ev0: Wander[P]): IndexedTraversal_[I, S, T, A, A] = {
        wander(new LensLikeIndexedTraversal[I, S, T, A, A] {
          override def apply[F[_]](f: ((I, A)) => F[A])(implicit ev: Applicative[F]): S => F[T] = {
            val starIndex: Indexed[Star[F, *, *], I, A, A] = Indexed[Star[F, *, *], I, A, A](Star {
              case (i, a) => if (pr(i)) f((i, a)) else ev.pure(a)
            })

            indexedTraversal(starIndex).runStar
          }
        })
    }
  }
}