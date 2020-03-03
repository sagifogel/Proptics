package proptics

import cats.{Applicative, Traverse}
import proptics.IndexedLens.liftIndexedOptic
import proptics.internal.{Indexed, Traversing, Wander}
import proptics.rank2types.{LensLikeIndexedTraversal, Rank2TypeIndexedTraversalLike}

import scala.Function.untupled

/**
 * An [[IndexedTraversal]] is An IndexedOptic constrained with [[Wander]] [[cats.arrow.Profunctor]]
 *
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
  private[proptics] def apply[I, S, T, A, B](f: Rank2TypeIndexedTraversalLike[I, S, T, A, B]): IndexedTraversal[I, S, T, A, B] = new IndexedTraversal[I, S, T, A, B] {
    override def apply[P[_, _]](indexed: Indexed[P, I, A, B])(implicit ev: Wander[P]): P[S, T] = f(indexed)
  }

  def apply[I, S, T, A, B](get: S => (I, A))(set: S => B => T): IndexedTraversal[I, S, T, A, B] = new IndexedTraversal[I, S, T, A, B] {
    override def apply[P[_, _]](indexed: Indexed[P, I, A, B])(implicit ev: Wander[P]): P[S, T] = {
      val traversing = new Traversing[S, T, (I, A), B] {
        override def apply[F[_]](f: ((I, A)) => F[B])(implicit ev: Applicative[F]): S => F[T] =
          s => ev.map(f(get(s)))(set(s))
      }

      ev.wander(traversing)(indexed.runIndex)
    }
  }

  def apply[I, S, T, A, B](to: S => ((I, A), B => T)): IndexedTraversal[I, S, T, A, B] =
    IndexedTraversal(new Rank2TypeIndexedTraversalLike[I, S, T, A, B] {
      override def apply[P[_, _]](indexed: Indexed[P, I, A, B])(implicit ev: Wander[P]): P[S, T] = {
        liftIndexedOptic(to)(ev)(indexed.runIndex)
      }
    })

  def fromTraverse[G[_], I, A, B](implicit ev0: Traverse[G]): IndexedTraversal[I, G[(I, A)], G[B], A, B] =
    IndexedTraversal(new Rank2TypeIndexedTraversalLike[I, G[(I, A)], G[B], A, B] {
      override def apply[P[_, _]](indexed: Indexed[P, I, A, B])(implicit ev1: Wander[P]): P[G[(I, A)], G[B]] = {
        val traversing = new Traversing[G[(I, A)], G[B], (I, A), B] {
          override def apply[F[_]](f: ((I, A)) => F[B])(implicit ev2: Applicative[F]): G[(I, A)] => F[G[B]] =
            ev0.traverse[F, (I, A), B](_)(f)
        }

        ev1.wander(traversing)(indexed.runIndex)
      }
    })

  def wander[I, S, T, A, B](itr: LensLikeIndexedTraversal[I, S, T, A, B]): IndexedTraversal[I, S, T, A, B] = {
    IndexedTraversal(new Rank2TypeIndexedTraversalLike[I, S, T, A, B] {
      override def apply[P[_, _]](indexed: Indexed[P, I, A, B])(implicit ev0: Wander[P]): P[S, T] = {
        def traversing = new Traversing[S, T, (I, A), B] {
          override def apply[F[_]](f: ((I, A)) => F[B])(implicit ev1: Applicative[F]): S => F[T] =
            itr[F](untupled(f).curried)
        }

        ev0.wander(traversing)(indexed.runIndex)
      }
    })
  }
}

object IndexedTraversal_ {
  def apply[I, S, A](get: S => (I, A))(set: S => A => S): IndexedTraversal_[I, S, A] = IndexedTraversal(get)(set)

  def apply[I, S, A](to: S => ((I, A), A => S)): IndexedTraversal_[I, S, A] = traversal(to)

  def traversal[I, S, A](to: S => ((I, A), A => S)): IndexedTraversal_[I, S, A] = IndexedTraversal[I, S, S, A, A](to)

  def fromTraverse[G[_], I, A](implicit ev0: Traverse[G]): IndexedTraversal[I, G[(I, A)], G[A], A, A] =
    IndexedTraversal.fromTraverse[G, I, A, A]

  def wander[I, S, A](itr: LensLikeIndexedTraversal[I, S, S, A, A]): IndexedTraversal_[I, S, A] =
    IndexedTraversal.wander[I, S, S, A, A](itr)
}