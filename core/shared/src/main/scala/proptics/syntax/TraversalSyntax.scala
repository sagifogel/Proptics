package proptics.syntax

import cats.Applicative
import cats.data.{Nested, State}
import cats.syntax.apply._
import cats.syntax.eq._

import proptics.Traversal_
import proptics.profunctor.{Star, Traversing, Wander}
import proptics.rank2types.Rank2TypeTraversalLike
import proptics.syntax.indexedTraversal._
import proptics.syntax.star._

trait TraversalSyntax {
  implicit final def traversalElementOps[S, T, A](traversal: Traversal_[S, T, A, A]): TraversalElementOps[S, T, A] = TraversalElementOps(traversal)

  implicit final def traversalFSequenceOps[F[_], S, T, A](traversal: Traversal_[S, T, F[A], A]): TraversalFSequenceOps[F, S, T, A] = TraversalFSequenceOps(traversal)
}

final case class TraversalElementOps[S, T, A](private val traversal: Traversal_[S, T, A, A]) extends AnyVal {
  /** narrow the focus to a single element */
  def element(i: Int): Traversal_[S, T, A, A] = filterByIndex(_ === i)

  /** traverse elements of a [[Traversal_]] whose index satisfy a predicate */
  def filterByIndex(predicate: Int => Boolean): Traversal_[S, T, A, A] =
    traversal.asIndexableTraversal.filterByIndex(predicate).unIndex

  /** take longest prefix of elements of a [[Traversal_]] that satisfy a predicate */
  def takeWhile(predicate: A => Boolean)(implicit ev0: Applicative[State[Boolean, *]]): Traversal_[S, T, A, A] =
    traverseWhile(predicate, take = true)

  /** drop longest prefix of elements of a [[Traversal_]] that satisfy a predicate */
  def dropWhile(predicate: A => Boolean)(implicit ev0: Applicative[State[Boolean, *]]): Traversal_[S, T, A, A] =
    traverseWhile(predicate, take = false)

  private[TraversalElementOps] def traverseWhile(predicate: A => Boolean, take: Boolean)(implicit ev0: Applicative[State[Boolean, *]]): Traversal_[S, T, A, A] =
    Traversal_(new Rank2TypeTraversalLike[S, T, A, A] {
      override def apply[P[_, _]](pab: P[A, A])(implicit ev1: Wander[P]): P[S, T] = {
        val traversing = new Traversing[S, T, A, A] {
          override def apply[F[_]](f: A => F[A])(s: S)(implicit ev2: Applicative[F]): F[T] = {
            val state: State[Boolean, Unit] = State.apply[Boolean, Unit](b => (b, ()))
            val starNested: Star[Nested[State[Boolean, *], F, *], A, A] = Star { a =>
              val composed = state.modify(_ && predicate(a)) *> (state.get, ev0.pure(a)).mapN { (b, a) =>
                if (b)
                  if (take) f(a) else ev2.pure(a)
                else if (take) ev2.pure(a)
                else f(a)
              }

              Nested(composed)
            }

            traversal(starNested)
              .runStar(s)
              .value
              .runA(true)
              .value
          }
        }

        ev1.wander(traversing)(pab)
      }
    })
}

final case class TraversalFSequenceOps[F[_], S, T, A](private val traversal: Traversal_[S, T, F[A], A]) extends AnyVal {
  /** invert a structure of S containing F[A] to F[T], a structure T containing A's inside an Applicative Functor */
  def sequence(s: S)(implicit ev: Applicative[F]): F[T] = traversal.traverse(s)(identity)
}
