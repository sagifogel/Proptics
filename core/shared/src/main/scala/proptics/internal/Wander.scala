package proptics.internal

import cats.arrow.Strong
import cats.{Applicative, Comonad, Id}
import proptics.newtype.Newtype
import proptics.newtype.Newtype.Aux
import proptics.profunctor.Choice.{choiceFunction, choiceStar}
import proptics.profunctor.Costar.{choiceCostar, profunctorCostar, strongCostar}
import proptics.profunctor.Star.strongStar
import proptics.profunctor.{Choice, Costar, Star}

trait Traversing[S, T, A, B] {
  def apply[F[_]](f: A => F[B])(s: S)(implicit ev: Applicative[F]): F[T]
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

    override def left[A, B, C](pab: A => B): Either[A, C] => Either[B, C] = choiceFunction.left(pab)

    override def right[A, B, C](pab: B => C): Either[A, B] => Either[A, C] = choiceFunction.right(pab)

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

    override def right[A, B, C](pab: Star[F, B, C]): Star[F, Either[A, B], Either[A, C]] = choiceStar.right(pab)

    override def first[A, B, C](fa: Star[F, A, B]): Star[F, (A, C), (B, C)] = strongStar.first(fa)

    override def second[A, B, C](fa: Star[F, A, B]): Star[F, (C, A), (C, B)] = strongStar.second(fa)

    override def dimap[A, B, C, D](fab: Star[F, A, B])(f: C => A)(g: B => D): Star[F, C, D] =
      choiceStar.dimap(fab)(f)(g)
  }

  implicit final def wanderCostar[F[_]](implicit ev0: Applicative[F], ev1: Comonad[F]): Wander[Costar[F, *, *]] = new Wander[Costar[F, *, *]] {
    override def wander[S, T, A, B](traversal: Traversing[S, T, A, B])(pab: Costar[F, A, B]): Costar[F, S, T] =
      Costar(fs => {
        val s2ft = traversal(ev0.pure[B] _ compose pab.runCostar compose ev0.pure)(_)
        val composed = ev0.pure(ev1.extract[T] _ compose s2ft)

        ev1.extract(ev0.ap(composed)(fs))
      })

    override def left[A, B, C](pab: Costar[F, A, B]): Costar[F, Either[A, C], Either[B, C]] = choiceCostar[F].left(pab)

    override def right[A, B, C](pab: Costar[F, B, C]): Costar[F, Either[A, B], Either[A, C]] = choiceCostar[F].right(pab)

    override def first[A, B, C](fa: Costar[F, A, B]): Costar[F, (A, C), (B, C)] = strongCostar[F].first(fa)

    override def second[A, B, C](fa: Costar[F, A, B]): Costar[F, (C, A), (C, B)] = strongCostar.second(fa)

    override def dimap[A, B, C, D](fab: Costar[F, A, B])(f: C => A)(g: B => D): Costar[F, C, D] =
      profunctorCostar[F](ev0).dimap(fab)(f)(g)
  }
}

object Wander extends WanderInstances
