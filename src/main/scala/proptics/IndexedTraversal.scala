package proptics

import cats.Applicative
import proptics.internal.{Indexed, Traversing, Wander}
import proptics.rank2types.{LensLikeIndexedTraversal, Rank2TypeIndexedTraversalLike}

import scala.Function.untupled

/**
 * An [[IndexedTraversal]] is An IndexedOptic constrained with [[Wander]] [[cats.arrow.Profunctor]]
 *
 * @tparam P an evidence of [[Wander]]
 * @tparam I the index of an [[IndexedTraversal]]
 * @tparam S the source of an [[IndexedTraversal]]
 * @tparam T the modified source of an [[IndexedTraversal]]
 * @tparam A the target of an [[IndexedTraversal]]
 * @tparam B the modified target of an [[IndexedTraversal]]
 */
abstract class IndexedTraversal[I, S, T, A, B] {
  def apply[P[_, _]](indexed: Indexed[P, I, A, B])(implicit ev: Wander[P]): P[S, T]
}

object IndexedTraversal {
  private[proptics] def apply[P[_, _], I, S, T, A, B](f: Rank2TypeIndexedTraversalLike[I, S, T, A, B]): IndexedTraversal[I, S, T, A, B] = new IndexedTraversal[I, S, T, A, B] {
    override def apply[P[_, _]](indexed: Indexed[P, I, A, B])(implicit ev: Wander[P]): P[S, T] = f(indexed)
  }

  def iwander[I, S, T, A, B](itr: LensLikeIndexedTraversal[I, S, T, A, B]): IndexedTraversal[I, S, T, A, B] = {
    IndexedTraversal(new Rank2TypeIndexedTraversalLike[I, S, T, A, B] {
      override def apply[P[_, _]](indexed: Indexed[P, I, A, B])(implicit ev: Wander[P]): P[S, T] = {
        def traversing = new Traversing[S, T, (I, A), B] {
          override def apply[F[_]](f: ((I, A)) => F[B])(implicit ev: Applicative[F]): S => F[T] =
            itr[F](untupled(f).curried)
        }

        ev.wander(traversing)(indexed.runIndex)
      }
    })
  }
}
