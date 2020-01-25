package proptics

import cats.{Applicative, Traverse}
import proptics.internal.{Traversing, Wander}

/**
 *
 * @tparam P an evidence of [[Wander]]
 * @tparam S the source of a [[Traversal]]
 * @tparam T the modified source of a [[Traversal]]
 * @tparam A the target of a [[Traversal]]
 * @tparam B the modified target of a [[Traversal]]
 */
abstract class Traversal[P[_, _] : Wander, S, T, A, B] extends Optic[P, S, T, A, B] {
}

object Traversal {
  import Lens.liftOptic

  private[proptics] def apply[P[_, _], S, T, A, B](f: P[A, B] => P[S, T])(implicit ev: Wander[P]): Traversal[P, S, T, A, B] = new Traversal[P, S, T, A, B] {
    override def apply(pab: P[A, B]): P[S, T] = f(pab)
  }

  def apply[P[_, _], S, T, A, B](get: S => A)(set: S => B => T)(implicit ev: Wander[P]): Traversal[P, S, T, A, B] = new Traversal[P, S, T, A, B] {
    override def apply(pab: P[A, B]): P[S, T] = {
      val traversing = new Traversing[S, T, A, B] {
        override def apply[F[_]](fab: A => F[B])(implicit ev: Applicative[F]): S => F[T] =
          s => ev.map(fab(get(s)))(set(s))
      }

      ev.wander(traversing)(pab)
    }
  }

  def apply[P[_, _], S, T, A, B](to: S => (A, B => T))(implicit ev: Wander[P], ev2: DummyImplicit): Traversal[P, S, T, A, B] =
    Traversal(liftOptic(to))

  def traversed[P[_, _], G[_], A, B](implicit ev0: Wander[P], ev1: Traverse[G]): Traversal[P, G[A], G[B], A, B] =
    Traversal((pab: P[A, B]) => {
      val traversing = new Traversing[G[A], G[B], A, B] {
        override def apply[F[_]](f: A => F[B])(implicit ev: Applicative[F]): G[A] => F[G[B]] =
          ev1.traverse[F, A, B](_)(f)
      }

      ev0.wander(traversing)(pab)
    })
}

object Traversal_ {
  def apply[P[_, _], S, A](get: S => A)(set: S => A => S)(implicit ev: Wander[P]): Traversal_[P, S, A] = Traversal(get)(set)

  def traversal[P[_, _], S, A](to: S => (A, A => S))(implicit ev: Wander[P]): Traversal_[P, S, A] = Traversal(to)
}
