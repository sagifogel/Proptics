package proptics.law

import scala.Function.const

import cats.laws._
import cats.syntax.profunctor._

import proptics.profunctor.Closed

trait ClosedLaws[F[_, _]] extends ProfunctorLaws[F] {
  implicit def F: Closed[F]

  def lmapClosedRmapClosedConsistent[A](fab: F[A, A]): IsEq[F[A => A, A => A]] =
    F.closed[A, A, A](fab).rmap[A => A](f => a => f(a)) <-> F.closed[A, A, A](fab).lmap[A => A](f => a => f(a))

  def closedComposeClosedDimapConsistent[A, B](fab: F[A, B]): IsEq[F[A => A => A, A => A => B]] =
    F.closed[A => A, A => B, A](F.closed[A, B, A](fab)) <->
      F.closed[A, B, (A, A)](fab).dimap[A => A => A, A => A => B](f => p => f(p._1)(p._2)) { f => a1 => a2 =>
        f((a1, a2))
      }

  def dimapConstIdentityConsistent[A, B](fab: F[A, B]): IsEq[F[A, B]] =
    F.closed[A, B, Unit](fab).dimap[A, B](const)(_.apply(())) <-> fab
}

object ClosedLaws {
  def apply[F[_, _]](implicit ev: Closed[F]): ClosedLaws[F] =
    new ClosedLaws[F] { def F: Closed[F] = ev }
}
