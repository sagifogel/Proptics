package proptics.internal

import cats.Functor
import cats.arrow.{Profunctor, Strong}
import cats.syntax.either._
import proptics.profunctor.Choice

/** The [[Stall]] [[cats.arrow.Profunctor]] characterizes an [[proptics.AffineTraversal_]] */
final case class Stall[A, B, S, T](getOrModify: S => Either[T, A], set: S => B => T)

abstract class StallInstances {
  implicit final def functorStall[C, D, E]: Functor[Stall[C, D, E, *]] = new Functor[Stall[C, D, E, *]] {
    override def map[A, B](fa: Stall[C, D, E, A])(f: A => B): Stall[C, D, E, B] =
      Stall(fa.getOrModify(_).leftMap(f), e => d => f(fa.set(e)(d)))
  }

  implicit final def profunctorStall[E, F]: Profunctor[Stall[E, F, *, *]] = new Profunctor[Stall[E, F, *, *]] {
    override def dimap[A, B, C, D](fab: Stall[E, F, A, B])(f: C => A)(g: B => D): Stall[E, F, C, D] =
      Stall(c => fab.getOrModify(f(c)).leftMap(g), c => ff => g(fab.set(f(c))(ff)))
  }

  implicit final def strongStall[E, F]: Strong[Stall[E, F, *, *]] = new Strong[Stall[E, F, *, *]] {
    override def first[A, B, C](fa: Stall[E, F, A, B]): Stall[E, F, (A, C), (B, C)] =
      Stall({ case (a, c) => fa.getOrModify(a).leftMap((_, c)) }, { case (a, c) => f => (fa.set(a)(f), c) })

    override def second[A, B, C](fa: Stall[E, F, A, B]): Stall[E, F, (C, A), (C, B)] =
      Stall({ case (c, a) => fa.getOrModify(a).leftMap((c, _)) }, { case (c, a) => f => (c, fa.set(a)(f)) })

    override def dimap[A, B, C, D](fab: Stall[E, F, A, B])(f: C => A)(g: B => D): Stall[E, F, C, D] =
      profunctorStall[E, F].dimap(fab)(f)(g)
  }

  implicit final def choiceStall[E, F]: Choice[Stall[E, F, *, *]] = new Choice[Stall[E, F, *, *]] {
    override def left[A, B, C](pab: Stall[E, F, A, B]): Stall[E, F, Either[A, C], Either[B, C]] =
      Stall(
        {
          case Left(a)  => pab.getOrModify(a).fold(_.asLeft[C].asLeft[E], _.asRight[Either[B, C]])
          case Right(c) => c.asRight[B].asLeft[E]
        },
        either => f => either.leftMap(pab.set(_)(f))
      )

    override def right[A, B, C](pab: Stall[E, F, B, C]): Stall[E, F, Either[A, B], Either[A, C]] =
      Stall(
        {
          case Left(a)  => a.asLeft[C].asLeft[E]
          case Right(b) => pab.getOrModify(b).fold(_.asRight[A].asLeft[E], _.asRight[Either[A, C]])
        },
        either => f => either.map(pab.set(_)(f))
      )

    override def dimap[A, B, C, D](fab: Stall[E, F, A, B])(f: C => A)(g: B => D): Stall[E, F, C, D] =
      profunctorStall[E, F].dimap(fab)(f)(g)
  }
}

object Stall extends StallInstances
