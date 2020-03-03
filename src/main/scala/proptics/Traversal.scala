package proptics

import cats.data.{Nested, State}
import cats.implicits._
import cats.{Applicative, Traverse}
import proptics.IndexedTraversal.wander
import proptics.internal.Wander.wanderStar
import proptics.internal.{Traversing, Wander}
import proptics.profunctor.Star
import proptics.rank2types.{LensLikeIndexedTraversal, Rank2TypeTraversalLike}
import Lens.liftOptic

import scala.Function.uncurried

/**
 *
 * @tparam S the source of a [[Traversal]]
 * @tparam T the modified source of a [[Traversal]]
 * @tparam A the target of a [[Traversal]]
 * @tparam B the modified target of a [[Traversal]]
 */
abstract class Traversal[S, T, A, B] { self =>
  def apply[P[_, _]](pab: P[A, B])(implicit ev: Wander[P]): P[S, T]

  def positions(implicit ev0: Applicative[State[Int, *]], ev1: State[Int, A]): IndexedTraversal[Int, S, T, A, B] = {
    wander(new LensLikeIndexedTraversal[Int, S, T, A, B] {
      override def apply[F[_]](f: Int => A => F[B])(implicit ev2: Applicative[F]): S => F[T] = s => {
        val starNested: Star[Nested[State[Int, *], F, *], A, B] = Star((a: A) => {
          val composed = (ev1.get, ev0.pure(a)).mapN(uncurried(f)) <* ev1.modify(_ + 1)

          Nested(composed)
        })

        val star: Star[Nested[State[Int, *], F, *], S, T] = self(starNested)
        val state: State[Int, F[T]] = star.runStar(s).value

        state.runA(0).value
      }
    })
  }
}

object Traversal {
  private[proptics] def apply[S, T, A, B](f: Rank2TypeTraversalLike[S, T, A, B]): Traversal[S, T, A, B] = new Traversal[S, T, A, B] {
    override def apply[P[_, _]](pab: P[A, B])(implicit ev: Wander[P]): P[S, T] = f(pab)
  }

  def apply[S, T, A, B](get: S => A)(set: S => B => T): Traversal[S, T, A, B] = new Traversal[S, T, A, B] {
    override def apply[P[_, _]](pab: P[A, B])(implicit ev: Wander[P]): P[S, T] = {
      val traversing = new Traversing[S, T, A, B] {
        override def apply[F[_]](f: A => F[B])(implicit ev: Applicative[F]): S => F[T] =
          s => ev.map(f(get(s)))(set(s))
      }

      ev.wander(traversing)(pab)
    }
  }

  def apply[S, T, A, B](to: S => (A, B => T)): Traversal[S, T, A, B] =
    Traversal(new Rank2TypeTraversalLike[S, T, A, B] {
      override def apply[P[_, _]](pab: P[A, B])(implicit ev: Wander[P]): P[S, T] =
        liftOptic(to)(ev)(pab)
    })

  def fromTraverse[G[_], A, B](implicit ev0: Traverse[G]): Traversal[G[A], G[B], A, B] =
    Traversal(new Rank2TypeTraversalLike[G[A], G[B], A, B] {
      override def apply[P[_, _]](pab: P[A, B])(implicit ev1: Wander[P]): P[G[A], G[B]] = {
        val traversing = new Traversing[G[A], G[B], A, B] {
          override def apply[F[_]](f: A => F[B])(implicit ev2: Applicative[F]): G[A] => F[G[B]] =
            ev0.traverse[F, A, B](_)(f)
        }

        ev1.wander(traversing)(pab)
      }
    })
}

object Traversal_ {
  def apply[S, A](get: S => A)(set: S => A => S): Traversal_[S, A] = Traversal(get)(set)

  def apply[S, A](to: S => (A, A => S)): Traversal_[S, A] = Traversal(to)

  def fromTraverse[G[_], A, B](implicit ev: Traverse[G]): Traversal[G[A], G[A], A, A] = Traversal.fromTraverse
}
