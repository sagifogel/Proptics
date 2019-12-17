package optics.internal

import cats.Applicative
import cats.arrow.Strong
import cats.syntax.either._
import optics.profunctor.{Choice, Star}

trait Traversal[S, T, A, B] {
  def apply[F[_]: Applicative](f: A => F[B]): S => F[T]
}

/** Class for profunctors that support polymorphic traversals */
trait Wander[P[_, _]] extends Strong[P] with Choice[P] {
  def wander[S, T, A, B](traversal: Traversal[S, T, A, B])(pab: P[A, B]): P[S, T]
}

abstract class WanderInstances {
  implicit def instance[F[_]](implicit ev: Applicative[F]): Wander[Star[F, *, *]] = new Wander[Star[F, *, *]] {
    override def wander[S, T, A, B](traversal: Traversal[S, T, A, B])(pab: Star[F, A, B]): Star[F, S, T] =
      Star(traversal(pab.runStar))

    override def left[A, B, C](pab: Star[F, A, B]): Star[F, Either[A, C], Either[B, C]] =
      Star {
        case Left(a) => ev.map(pab.runStar(a))(_.asLeft[C])
        case Right(c) => ev.pure(c.asRight[B])
      }

    override def right[A, B, C](pab: Star[F, B, C]): Star[F, Either[A, B], Either[A, C]] =
      Star {
        case Left(a) => ev.pure(a.asLeft[C])
        case Right(b) => ev.map(pab.runStar(b))(_.asRight[A])
      }

    override def first[A, B, C](fa: Star[F, A, B]): Star[F, (A, C), (B, C)] =
      Star { case (a, c) => ev.map(fa.runStar(a))((_, c)) }

    override def second[A, B, C](fa: Star[F, A, B]): Star[F, (C, A), (C, B)] =
      Star { case (c, a) => ev.map(fa.runStar(a))((c, _)) }

    override def dimap[A, B, C, D](fab: Star[F, A, B])(f: C => A)(g: B => D): Star[F, C, D] = {
      Star(ev.lift(g) compose fab.runStar compose f)
    }
  }
}

object Wander extends WanderInstances
