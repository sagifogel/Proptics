package optics.internal

import cats.Applicative
import cats.arrow.{Choice, Strong}

/** Class for profunctors that support polymorphic traversals */
trait Wander[P[_, _]] {
  def wander[F[_], S, T, A, B](f: (A => F[B]) => S => F[T])
                              (pab: P[A, B])
                              (implicit ev: Strong[P],
                               ev2: Choice[P],
                               ev3: Applicative[F]): P[S, T]
}


abstract class WanderInstances {
}

object Wander extends WanderInstances
