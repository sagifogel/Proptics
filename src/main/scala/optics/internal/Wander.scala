package optics.internal

import cats.Applicative
import cats.arrow.{Choice, Strong}
import optics.profunctor.Star
/**
 * Class for profunctors that support polymorphic traversals
 */
trait Wander[P[_, _]] {
  def wander[F[_], S, T, A, B](f: (A => F[B]) => S => F[T])
                              (pab: P[A, B])
                              (implicit ev: Strong[P],
                               ev2: Choice[P],
                               ev3: Applicative[F]): P[S, T]
}

abstract class WanderInstances {
  implicit def wanderFunction1: Wander[* => *] = new Wander[* => *] {
    override def wander[F[_], S, T, A, B](f: (A => F[B]) => S => F[T])
                                         (pab: A => B)
                                         (implicit ev: Strong[* => *],
                                          ev2: Choice[* => *],
                                          ev3: Applicative[F]): S => T = ???
  }

  implicit def wanderStar[FF[_]]: Wander[Star[FF, *, *]] = new Wander[Star[FF, *, *]] {
    override def wander[F[_], S, T, A, B](f: (A => F[B]) => S => F[T])
                                         (pab: Star[FF, A, B])
                                         (implicit ev: Strong[Star[FF, *, *]],
                                          ev2: Choice[Star[FF, *, *]],
                                          ev3: Applicative[F]): Star[FF, S, T] = ???
  }
}

object Wander extends WanderInstances
