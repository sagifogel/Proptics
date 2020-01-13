package optics.internal

import cats.arrow.Strong
import cats.syntax.either._
import cats.{Applicative, Id}
import optics.newtype.Newtype
import optics.newtype.Newtype.Aux
import optics.profunctor.{Choice, Star}

trait Traversing[S, T, A, B] {
  def apply[F[_]](f: A => F[B])(implicit ev: Applicative[F]): S => F[T]
}

/** Class for profunctors that support polymorphic traversals */
trait Wander[P[_, _]] extends Strong[P] with Choice[P] {
  def wander[S, T, A, B](traversal: Traversing[S, T, A, B])(pab: P[A, B]): P[S, T]
}

abstract class WanderInstances {
  import Newtype.{alaF, newtype}

  implicit final def wanderFunction: Wander[* => *] = new Wander[* => *] {
    override def wander[S, T, A, B](traversal: Traversing[S, T, A, B])(pab: A => B): S => T = {
      implicit val newTypeS: Aux[S, S] = newtype(identity[S])(identity[S])
      implicit val newTypeT: Aux[T, T] = newtype(identity[T])(identity[T])

      alaF[Id, Id, S, S, T, T](identity)(traversal[Id](pab))
    }

    override def left[A, B, C](pab: A => B): Either[A, C] => Either[B, C] = _.leftMap(pab)

    override def right[A, B, C](pab: B => C): Either[A, B] => Either[A, C] = _.map(pab)

    override def first[A, B, C](fa: A => B): ((A, C)) => (B, C) = {
      case (a, c) => (fa(a), c)
    }

    override def second[A, B, C](fa: A => B): ((C, A)) => (C, B) = {
      case (c, a) => (c, fa(a))
    }

    override def dimap[A, B, C, D](fab: A => B)(f: C => A)(g: B => D): C => D = g compose fab compose f
  }

  implicit final def wanderStar[F[_]](implicit ev: Applicative[F]): Wander[Star[F, *, *]] = new Wander[Star[F, *, *]] {
    override def wander[S, T, A, B](traversal: Traversing[S, T, A, B])(pab: Star[F, A, B]): Star[F, S, T] =
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
