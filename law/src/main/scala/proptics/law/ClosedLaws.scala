package proptics.law

import cats.laws._
import cats.syntax.profunctor._
import proptics.profunctor.Closed
import Function.const

trait ClosedLaws[F[_, _]] extends ProfunctorLaws[F] {
  implicit def F: Closed[F]

  def lmapClosedRmapClosedConsistent[A, B, C](fab: F[A, B], f: (C => B) => C => B, g: (C => A) => C => A): IsEq[F[C => A, C => B]] =
    F.closed[A, B, C](fab).rmap[C => B](f) <-> F.closed[A, B, C](fab).lmap[C => A](g)

  def closedComposeClosedDimapConsistent[A, B, C](fab: F[A, B]): IsEq[F[A => A => A, A => A => B]] =
    F.closed[A => A, A => B, A](F.closed[A, B, A](fab)) <->
      F.closed[A, B, (A, A)](fab).dimap[A => A => A, A => A => B](f => p => f(p._1)(p._2)) { f => a1 => a2 =>
        f((a1, a2))
      }

  def dimapConstIdentityConsistent[A, B, C](fab: F[A, B]): IsEq[F[A, B]] =
    F.closed[A, B, Unit](fab).dimap[A, B](const)(_.apply(())) <-> fab
}

object ClosedLaws {
  def apply[F[_, _]](implicit ev: Closed[F]): ClosedLaws[F] =
    new ClosedLaws[F] { def F: Closed[F] = ev }
}
