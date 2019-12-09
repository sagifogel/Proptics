package optics.internal

import cats.Applicative
import cats.arrow.{Choice, Strong}

trait Wander[P[_, _]] {
  def wander[F[_], S, T, A, B](f: (A => F[B]) => S => F[T])
                              (implicit ev: Strong[P], ev2: Choice[P], ev3: Applicative[F]): P[A, B] => P[S, T]
}

abstract class WanderInstances {
}

object Wander extends WanderInstances
