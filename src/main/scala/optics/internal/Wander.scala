package optics.internal

import cats.Applicative
import cats.arrow.Strong
import optics.profunctor.{Choice, Star}

trait Traversal[S, T, A, B] {
  def apply[F[_]: Applicative](f: A => F[B]): S => F[T]
}

/** Class for profunctors that support polymorphic traversals */
trait Wander[P[_, _]] extends Strong[P] with Choice[P] {
  def wander[S, T, A, B](traversal: Traversal[S, T, A, B])(pab: P[A, B]): P[S, T]
}

abstract class WanderInstances {
  implicit def instance[F[_]: Applicative]: Wander[Star[F, *, *]] = new Wander[Star[F, *, *]] {
    override def wander[S, T, A, B](traversal: Traversal[S, T, A, B])(pab: Star[F, A, B]): Star[F, S, T] =
      Star(traversal(pab.runStar))

    override def left[A, B, C](pab: Star[F, A, B]): Star[F, Either[A, C], Either[B, C]] = ???

    override def right[A, B, C](pab: Star[F, B, C]): Star[F, Either[A, B], Either[A, C]] = ???

    override def first[A, B, C](fa: Star[F, A, B]): Star[F, (A, C), (B, C)] = ???

    override def second[A, B, C](fa: Star[F, A, B]): Star[F, (C, A), (C, B)] = ???

    override def dimap[A, B, C, D](fab: Star[F, A, B])(f: C => A)(g: B => D): Star[F, C, D] = ???
  }
}

object Wander extends WanderInstances
