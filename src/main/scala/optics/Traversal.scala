package optics

import cats.Applicative
import optics.internal.{Traversing, Wander}

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
  def apply[P[_, _], S, T, A, B](implicit ev: Wander[P]): Traversal[P, S, T, A, B] = new Traversal[P, S, T, A, B] {
    override def apply(pab: P[A, B]): P[S, T] = {
      val traversing = new Traversing[S, T, A, B] {
        override def apply[F[_]](fab: A => F[B])(implicit ev: Applicative[F]): S => F[T] = ???
      }

      ev.wander(traversing)(pab)
    }
  }
}
