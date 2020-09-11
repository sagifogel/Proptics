package proptics.internal

import cats.arrow.Strong
import cats.{Applicative, Id}
import proptics.newtype.Newtype
import proptics.newtype.Newtype.newtypeId
import proptics.profunctor.Choice.{choiceFunction, choiceStar}
import proptics.profunctor.Star.strongStar
import proptics.profunctor.{Choice, Star}
import proptics.rank2types.Traversing

/** Class for profunctors that support polymorphic traversals */
trait Wander[P[_, _]] extends Strong[P] with Choice[P] {
  def wander[S, T, A, B](traversal: Traversing[S, T, A, B])(pab: P[A, B]): P[S, T]
}

abstract class WanderInstances {
  implicit final def wanderFunction: Wander[* => *] = new Wander[* => *] {
    override def wander[S, T, A, B](traversal: Traversing[S, T, A, B])(pab: A => B): S => T =
      Newtype.alaF[Id, Id, S, S, T, T](traversal[Id](pab))

    override def left[A, B, C](pab: A => B): Either[A, C] => Either[B, C] = choiceFunction.left(pab)

    override def right[A, B, C](pab: A => B): Either[C, A] => Either[C, B] = choiceFunction.right(pab)

    override def first[A, B, C](fa: A => B): ((A, C)) => (B, C) = {
      case (a, c) => (fa(a), c)
    }

    override def second[A, B, C](fa: A => B): ((C, A)) => (C, B) = {
      case (c, a) => (c, fa(a))
    }

    override def dimap[A, B, C, D](fab: A => B)(f: C => A)(g: B => D): C => D = choiceFunction.dimap(fab)(f)(g)
  }

  implicit final def wanderStar[F[_]](implicit ev: Applicative[F]): Wander[Star[F, *, *]] = new Wander[Star[F, *, *]] {
    override def wander[S, T, A, B](traversal: Traversing[S, T, A, B])(pab: Star[F, A, B]): Star[F, S, T] =
      Star(traversal(pab.runStar))

    override def left[A, B, C](pab: Star[F, A, B]): Star[F, Either[A, C], Either[B, C]] = choiceStar.left(pab)

    override def right[A, B, C](pab: Star[F, A, B]): Star[F, Either[C, A], Either[C, B]] = choiceStar.right(pab)

    override def first[A, B, C](fa: Star[F, A, B]): Star[F, (A, C), (B, C)] = strongStar.first(fa)

    override def second[A, B, C](fa: Star[F, A, B]): Star[F, (C, A), (C, B)] = strongStar.second(fa)

    override def dimap[A, B, C, D](fab: Star[F, A, B])(f: C => A)(g: B => D): Star[F, C, D] =
      choiceStar.dimap(fab)(f)(g)
  }
}

object Wander extends WanderInstances
