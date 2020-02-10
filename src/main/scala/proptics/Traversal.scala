package proptics

import cats.{Applicative, Traverse}
import proptics.internal.{Traversing, Wander}
import proptics.rank2types.Rank2TypeTraversalLike

/**
 *
 * @tparam S the source of a [[Traversal]]
 * @tparam T the modified source of a [[Traversal]]
 * @tparam A the target of a [[Traversal]]
 * @tparam B the modified target of a [[Traversal]]
 */
abstract class Traversal[S, T, A, B] {
  def apply[P[_, _]](pab: P[A, B])(implicit ev: Wander[P]): P[S, T]
}

object Traversal {
  import Lens.liftOptic

  private[proptics] def apply[S, T, A, B](f: Rank2TypeTraversalLike[S, T, A, B]): Traversal[S, T, A, B] = new Traversal[S, T, A, B] {
    override def apply[P[_, _]](pab: P[A, B])(implicit ev: Wander[P]): P[S, T] = f(pab)
  }

  def apply[S, T, A, B](get: S => A)(set: S => B => T): Traversal[S, T, A, B] = new Traversal[S, T, A, B] {
    override def apply[P[_, _]](pab: P[A, B])(implicit ev: Wander[P]): P[S, T] = {
      val traversing = new Traversing[S, T, A, B] {
        override def apply[F[_]](fab: A => F[B])(implicit ev: Applicative[F]): S => F[T] =
          s => ev.map(fab(get(s)))(set(s))
      }

      ev.wander(traversing)(pab)
    }
  }

  def apply[S, T, A, B](to: S => (A, B => T)): Traversal[S, T, A, B] =
    Traversal(new Rank2TypeTraversalLike[S, T, A, B] {
      override def apply[P[_, _]](pab: P[A, B])(implicit ev: Wander[P]): P[S, T] =
        liftOptic(to)(ev)(pab)
    })

  def traversed[G[_], A, B](implicit ev1: Traverse[G]): Traversal[G[A], G[B], A, B] =
    Traversal(new Rank2TypeTraversalLike[G[A], G[B], A, B] {
      override def apply[P[_, _]](pab: P[A, B])(implicit ev0: Wander[P]): P[G[A], G[B]] = {
        val traversing = new Traversing[G[A], G[B], A, B] {
          override def apply[F[_]](f: A => F[B])(implicit ev: Applicative[F]): G[A] => F[G[B]] =
            ev1.traverse[F, A, B](_)(f)
        }

        ev0.wander(traversing)(pab)
      }
    })
}

object Traversal_ {
  def apply[S, A](get: S => A)(set: S => A => S): Traversal_[S, A] = Traversal(get)(set)

  def traversal[S, A](to: S => (A, A => S)): Traversal_[S, A] = Traversal(to)
}
