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
  def element(i: I)(implicit ev: Eq[I]): Traversal_[S, T, A, A] = filterByIndex(_ === i).unIndex

  /** traverse elements of an [[IndexedTraversal_]] whose index satisfy a predicate applied on the index */
  def filterByIndex(predicate: I => Boolean): IndexedTraversal_[I, S, T, A, A] =
    filter(predicate compose Tuple2._2)

  /** traverse elements of an [[IndexedTraversal_]] whose index satisfy a predicate */
  def filter(predicate: ((A, I)) => Boolean): IndexedTraversal_[I, S, T, A, A] =
    wander(new LensLikeWithIndex[I, S, T, A, A] {
      override def apply[F[_]](f: ((A, I)) => F[A])(implicit ev: Applicative[F]): S => F[T] = {
        val starIndex: Indexed[Star[F, *, *], I, A, A] = Indexed[Star[F, *, *], I, A, A](Star { case (a, i) =>
          if (predicate((a, i))) f((a, i)) else ev.pure(a)
        })

        indexedTraversal(starIndex).runStar
      }
    })

  /** take longest prefix of elements of an [[IndexedTraversal_]] that satisfy a predicate */
  def takeWhile(predicate: A => Boolean)(implicit ev0: Applicative[State[Boolean, *]]): IndexedTraversal_[I, S, T, A, A] =
    traverseWhile(predicate compose Tuple2._1, take = true)

  /** drop longest prefix of elements of an [[IndexedTraversal_]] that satisfy a predicate */
  def dropWhile(predicate: A => Boolean)(implicit ev0: Applicative[State[Boolean, *]]): IndexedTraversal_[I, S, T, A, A] =
    traverseWhile(predicate compose Tuple2._1, take = false)

  /** take longest prefix of elements of an [[IndexedTraversal_]] that satisfy a predicate */
  def takeWhileWithIndex(predicate: ((A, I)) => Boolean)(implicit ev0: Applicative[State[Boolean, *]]): IndexedTraversal_[I, S, T, A, A] =
    traverseWhile(predicate, take = true)

  /** drop longest prefix of elements of an [[IndexedTraversal_]] that satisfy a predicate */
  def dropWhileWithIndex(predicate: ((A, I)) => Boolean)(implicit ev0: Applicative[State[Boolean, *]]): IndexedTraversal_[I, S, T, A, A] =
    traverseWhile(predicate, take = false)

  private[IndexedTraversalOps] def traverseWhile(predicate: ((A, I)) => Boolean, take: Boolean)(implicit ev0: Applicative[State[Boolean, *]]): IndexedTraversal_[I, S, T, A, A] =
    IndexedTraversal_(new Rank2TypeIndexedTraversalLike[I, S, T, A, A] {
      override def apply[P[_, _]](pab: Indexed[P, I, A, A])(implicit ev1: Wander[P]): P[S, T] = {
        val traversing: Traversing[S, T, (A, I), A] = new Traversing[S, T, (A, I), A] {
          override def apply[F[_]](f: ((A, I)) => F[A])(s: S)(implicit ev2: Applicative[F]): F[T] = {
            val state: State[Boolean, Unit] = State.apply[Boolean, Unit](b => (b, ()))
            val starNested: Star[Nested[State[Boolean, *], F, *], (A, I), A] = Star { case pair @ (a, i) =>
              val composed = state.modify(_ && predicate(pair)) *> (state.get, ev0.pure(a)).mapN { (b, a) =>
                if (b)
                  if (take) f((a, i)) else ev2.pure(a)
                else if (take) ev2.pure(a)
                else f((a, i))
              }

              Nested(composed)
            }

            indexedTraversal(Indexed(starNested))
              .runStar(s)
              .value
              .runA(true)
              .value
          }
        }

        ev1.wander(traversing)(pab.runIndex)
      }
    })
}

final case class IndexedTraversalSequenceOps[F[_], I, S, T, A](private val iso: IndexedTraversal_[I, S, T, F[A], A]) extends AnyVal {
  /** invert a structure of S containing F[(I, A)] to F[T], a structure T containing A's inside an Applicative Functor */
  def sequence(s: S)(implicit ev: Applicative[F]): F[T] = iso.traverse(s)(_._1)
}
