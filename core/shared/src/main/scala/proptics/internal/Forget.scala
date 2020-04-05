package proptics.internal

import cats.arrow.{Profunctor, Strong}
import cats.data.Const
import cats.data.Const.catsDataApplicativeForConst
import cats.syntax.semigroup._
import cats.{Monoid, Semigroup}
import proptics.profunctor.{Choice, Closed, Cochoice}

import scala.Function.const

/**
 * [[cats.arrow.Profunctor]] that forgets the `B` value and returns (and accumulates) a value of type `R`.
 * [[Forget]] `R` is isomorphic to [[proptics.profunctor.Star (Const R)]], but can be given a [[Cochoice]] instance.
 */
final case class Forget[R, A, B](runForget: A => R)

abstract class ForgetInstances {
  implicit final def semigroupForget[R, A, B](implicit ev: Semigroup[R]): Semigroup[Forget[R, A, B]] = new Semigroup[Forget[R, A, B]] {
    override def combine(x: Forget[R, A, B], y: Forget[R, A, B]): Forget[R, A, B] =
      Forget(r => x.runForget(r) |+| y.runForget(r))
  }

  implicit final def monoidForget[R, A, B](implicit ev: Monoid[R]): Monoid[Forget[R, A, B]] = new Monoid[Forget[R, A, B]] {
    override def empty: Forget[R, A, B] = Forget(const(ev.empty))

    override def combine(x: Forget[R, A, B], y: Forget[R, A, B]): Forget[R, A, B] = x |+| y
  }

  implicit final def profunctorForget[R]: Profunctor[Forget[R, *, *]] = new Profunctor[Forget[R, *, *]] {
    override def dimap[A, B, C, D](fab: Forget[R, A, B])(f: C => A)(g: B => D): Forget[R, C, D] =
      Forget(fab.runForget compose f)
  }

  implicit final def choiceForget[R](implicit ev: Monoid[R]): Choice[Forget[R, *, *]] = new Choice[Forget[R, *, *]] {
    override def left[A, B, C](pab: Forget[R, A, B]): Forget[R, Either[A, C], Either[B, C]] =
      Forget(_.fold(pab.runForget, const(ev.empty)))

    override def right[A, B, C](pab: Forget[R, B, C]): Forget[R, Either[A, B], Either[A, C]] =
      Forget(_.fold(const(ev.empty), pab.runForget))

    override def dimap[A, B, C, D](fab: Forget[R, A, B])(f: C => A)(g: B => D): Forget[R, C, D] =
      profunctorForget.dimap(fab)(f)(g)
  }

  implicit final def strongForget[R](implicit P: Profunctor[Forget[R, *, *]]): Strong[Forget[R, *, *]] = new Strong[Forget[R, *, *]] {
    override def first[A, B, C](fa: Forget[R, A, B]): Forget[R, (A, C), (B, C)] =
      Forget { case (a, _) => fa.runForget(a) }

    override def second[A, B, C](fa: Forget[R, A, B]): Forget[R, (C, A), (C, B)] = {
      Forget { case (_, a) => fa.runForget(a) }
    }

    override def dimap[A, B, C, D](fab: Forget[R, A, B])(f: C => A)(g: B => D): Forget[R, C, D] =
      P.dimap(fab)(f)(g)
  }

  implicit final def cochoiceForget[R](implicit ev: Monoid[R]): Cochoice[Forget[R, *, *]] = new Cochoice[Forget[R, *, *]] {
    override def unleft[A, B, C](p: Forget[R, Either[A, C], Either[B, C]]): Forget[R, A, B] =
      Forget(p.runForget compose Left[A, C])

    override def unright[A, B, C](p: Forget[R, Either[A, B], Either[A, C]]): Forget[R, B, C] =
      Forget(p.runForget compose Right[A, B])

    override def dimap[A, B, C, D](fab: Forget[R, A, B])(f: C => A)(g: B => D): Forget[R, C, D] =
      profunctorForget.dimap(fab)(f)(g)
  }

  implicit final def wanderForget[R: Monoid]: Wander[Forget[R, *, *]] = new Wander[Forget[R, *, *]] {
    override def wander[S, T, A, B](traversal: Traversing[S, T, A, B])(pab: Forget[R, A, B]): Forget[R, S, T] = {
      Forget(traversal[Const[R, *]](Const[R, B] _ compose pab.runForget)(_).getConst)
    }

    override def first[A, B, C](fa: Forget[R, A, B]): Forget[R, (A, C), (B, C)] = {
      strongForget[R](profunctorForget).first(fa)
    }

    override def second[A, B, C](fa: Forget[R, A, B]): Forget[R, (C, A), (C, B)] =
      strongForget[R](profunctorForget).second(fa)

    override def left[A, B, C](pab: Forget[R, A, B]): Forget[R, Either[A, C], Either[B, C]] =
      choiceForget[R].left(pab)

    override def right[A, B, C](pab: Forget[R, B, C]): Forget[R, Either[A, B], Either[A, C]] =
      choiceForget[R].right(pab)

    override def dimap[A, B, C, D](fab: Forget[R, A, B])(f: C => A)(g: B => D): Forget[R, C, D] =
      profunctorForget.dimap(fab)(f)(g)
  }

  implicit final def closedForget[R](implicit ev: Monoid[R]): Closed[Forget[R, *, *]] = new Closed[Forget[R, *, *]] {
    override def closed[A, B, C](pab: Forget[R, A, B]): Forget[R, C => A, C => B] = Forget(const(ev.empty))

    override def dimap[A, B, C, D](fab: Forget[R, A, B])(f: C => A)(g: B => D): Forget[R, C, D] =
      profunctorForget.dimap(fab)(f)(g)
  }
}

object Forget extends ForgetInstances