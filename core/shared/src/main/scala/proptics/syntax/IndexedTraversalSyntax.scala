package proptics.syntax

import cats.data.{Nested, State}
import cats.syntax.apply._
import cats.syntax.eq._
import cats.{Applicative, Eq}

import proptics.IndexedTraversal_.wander
import proptics.internal.Indexed
import proptics.profunctor.{Star, Traversing, Wander}
import proptics.rank2types.{LensLikeWithIndex, Rank2TypeIndexedTraversalLike}
import proptics.syntax.star._
import proptics.syntax.tuple._
import proptics.{IndexedTraversal_, Traversal_}

trait IndexedTraversalSyntax {
  implicit def indexedTraversalOps[I, S, T, A](indexedTraversal: IndexedTraversal_[I, S, T, A, A]): IndexedTraversalOps[I, S, T, A] =
    IndexedTraversalOps(indexedTraversal)

  implicit def indexedTraversalSequenceOps[F[_], I, S, T, A](indexedTraversal: IndexedTraversal_[I, S, T, F[A], A]): IndexedTraversalSequenceOps[F, I, S, T, A] =
    IndexedTraversalSequenceOps(indexedTraversal)
}

final case class IndexedTraversalOps[I, S, T, A](private val indexedTraversal: IndexedTraversal_[I, S, T, A, A]) extends AnyVal {
  /** combine an index and an [[IndexedTraversal_]] to narrow the focus to a single element */
  def element(i: I)(implicit ev: Eq[I]): IndexedTraversal_[I, S, T, A, A] = filterByIndex(_ === i)

  /** traverse elements of an [[IndexedTraversal_]] whose index satisfy a predicate applied on the index */
  def filterByIndex(predicate: I => Boolean): IndexedTraversal_[I, S, T, A, A] =
    filter(predicate compose Tuple2._1)

  /** traverse elements of an [[IndexedTraversal_]] whose index satisfy a predicate */
  def filter(predicate: ((I, A)) => Boolean): IndexedTraversal_[I, S, T, A, A] =
    wander(new LensLikeWithIndex[I, S, T, A, A] {
      override def apply[F[_]](f: ((I, A)) => F[A])(implicit ev: Applicative[F]): S => F[T] = {
        val starIndex: Indexed[Star[F, *, *], I, A, A] = Indexed[Star[F, *, *], I, A, A](Star { case (i, a) =>
          if (predicate((i, a))) f((i, a)) else ev.pure(a)
        })

        indexedTraversal(starIndex).runStar
      }
    })
}

final case class IndexedTraversalSequenceOps[F[_], I, S, T, A](private val iso: IndexedTraversal_[I, S, T, F[A], A]) extends AnyVal {
  /** invert a structure of S containing F[(I, A)] to F[T], a structure T containing A's inside an Applicative Functor */
  def sequence(s: S)(implicit ev: Applicative[F]): F[T] = iso.traverse(s)(_._1)
}
