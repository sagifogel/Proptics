package proptics

import cats.Applicative
import proptics.internal.{Indexed, Traversing, Wander}

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
abstract class IndexedTraversal[P[_, _] : Wander, I, S, T, A, B] extends IndexedOptic[P, I, S, T, A, B] {
}

trait LensLikeIndexedTraversal[I, S, T, A, B] {
  def apply[F[_]](f: I => A => F[B])(implicit ev:Applicative[F]): S => F[T]
}

object IndexedTraversal {
  def apply[P[_, _], I, S, T, A, B](f: P[(I, A), B] => P[S, T])(implicit ev: Wander[P]): IndexedTraversal[P, I, S, T, A, B] = new IndexedTraversal[P, I, S, T, A, B] {
    override def apply(index: Indexed[P, I, A, B]): P[S, T] = f(index.runIndex)
  }

  def iwander[P[_, _], I, S, T, A, B](itr: LensLikeIndexedTraversal[I, S, T, A, B])(implicit ev: Wander[P]): IndexedTraversal[P, I, S, T, A, B] = {
    IndexedTraversal(piab => {
      def traversing = new Traversing[S, T, (I, A), B] {
        override def apply[F[_]](f: ((I, A)) => F[B])(implicit ev: Applicative[F]): S => F[T] =
          itr[F](untupled(f).curried)
      }

      ev.wander(traversing)(piab)
    })
  }
}
