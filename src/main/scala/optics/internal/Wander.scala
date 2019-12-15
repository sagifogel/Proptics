package optics.internal

import cats.Applicative
import cats.arrow.Strong
import optics.profunctor.{Choice, Star}

trait Traversal[S, T, A, B] {
  def apply[F[_]: Applicative](f: A => F[B]): S => F[T]
}

/** Class for profunctors that support polymorphic traversals */
trait Wander[P[_, _]] {
  def wander[S, T, A, B](traversal: Traversal[S, T, A, B])
                         (pab: P[A, B])
                         (implicit ev: Strong[P],
                         ev2: Choice[P]): P[S, T]
}

abstract class WanderInstances {
  implicit def instance[F[_]: Applicative]: Wander[Star[F, *, *]] = new Wander[Star[F, *, *]] {
    override def wander[S, T, A, B](traversal: Traversal[S, T, A, B])
                                   (pab: Star[F, A, B])
                                   (implicit ev: Strong[Star[F, *, *]],
                                    ev2: Choice[Star[F, *, *]]): Star[F, S, T] =
      Star(traversal(pab.runStar))
  }
}

object Wander extends WanderInstances
