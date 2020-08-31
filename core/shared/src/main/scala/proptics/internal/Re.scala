package proptics.internal

import cats.arrow.{Profunctor, Strong}
import proptics.profunctor.{Choice, Cochoice, Costrong}

/** Re is a class for optics that can be reversed. */
final case class Re[P[_, _], S, T, A, B](runRe: P[B, A] => P[T, S]) extends AnyVal

abstract class ReInstances {
  implicit final def profunctorRe[P[_, _], S, T](implicit ev: Profunctor[P]): Profunctor[Re[P, S, T, *, *]] = new Profunctor[Re[P, S, T, *, *]] {
    override def dimap[A, B, C, D](fab: Re[P, S, T, A, B])(f: C => A)(g: B => D): Re[P, S, T, C, D] =
      Re(pdc => fab.runRe(ev.dimap(pdc)(g)(f)))
  }

  implicit final def choiceRe[P[_, _], S, T](implicit ev: Choice[P]): Cochoice[Re[P, S, T, *, *]] = new Cochoice[Re[P, S, T, *, *]] {
    override def unleft[A, B, C](p: Re[P, S, T, Either[A, C], Either[B, C]]): Re[P, S, T, A, B] =
      Re(p.runRe compose ev.left)

    override def dimap[A, B, C, D](fab: Re[P, S, T, A, B])(f: C => A)(g: B => D): Re[P, S, T, C, D] =
      profunctorRe[P, S, T].dimap(fab)(f)(g)
  }

  implicit final def cochoiceRe[P[_, _], S, T](implicit ev: Cochoice[P]): Choice[Re[P, S, T, *, *]] = new Choice[Re[P, S, T, *, *]] {
    override def left[A, B, C](pab: Re[P, S, T, A, B]): Re[P, S, T, Either[A, C], Either[B, C]] =
      Re(pab.runRe compose ev.unleft)

    override def right[A, B, C](pab: Re[P, S, T, A, B]): Re[P, S, T, Either[C, A], Either[C, B]] =
      Re(pab.runRe compose ev.unright)

    override def dimap[A, B, C, D](fab: Re[P, S, T, A, B])(f: C => A)(g: B => D): Re[P, S, T, C, D] =
      profunctorRe[P, S, T].dimap(fab)(f)(g)
  }

  implicit final def strongRe[P[_, _], S, T](implicit ev: Strong[P]): Costrong[Re[P, S, T, *, *]] = new Costrong[Re[P, S, T, *, *]] {
    override def unfirst[A, B, C](p: Re[P, S, T, (A, C), (B, C)]): Re[P, S, T, A, B] =
      Re(p.runRe compose ev.first)

    override def unsecond[A, B, C](p: Re[P, S, T, (A, B), (A, C)]): Re[P, S, T, B, C] =
      Re(p.runRe compose ev.second)

    override def dimap[A, B, C, D](fab: Re[P, S, T, A, B])(f: C => A)(g: B => D): Re[P, S, T, C, D] =
      profunctorRe[P, S, T].dimap(fab)(f)(g)
  }

  implicit final def costrongRe[P[_, _], S, T](implicit ev: Costrong[P]): Strong[Re[P, S, T, *, *]] = new Strong[Re[P, S, T, *, *]] {
    override def first[A, B, C](fa: Re[P, S, T, A, B]): Re[P, S, T, (A, C), (B, C)] =
      Re(fa.runRe compose ev.unfirst)

    override def second[A, B, C](fa: Re[P, S, T, A, B]): Re[P, S, T, (C, A), (C, B)] =
      Re(fa.runRe compose ev.unsecond)

    override def dimap[A, B, C, D](fab: Re[P, S, T, A, B])(f: C => A)(g: B => D): Re[P, S, T, C, D] =
      profunctorRe[P, S, T].dimap(fab)(f)(g)
  }
}

object Re extends ReInstances
