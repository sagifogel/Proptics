package proptics.profunctor

import cats.arrow.{Category, Compose, Profunctor, Strong}
import cats.data.Cokleisli
import cats.syntax.either._
import cats.{Applicative, Apply, CoflatMap, Comonad, Distributive, FlatMap, Functor, Invariant, Monad, ~>}
import proptics.profunctor.Costar.hoistCostar

import scala.Function.const

/**
 *
 * [[Costar]] turns a [[Functor]] into a [[Profunctor]] "backwards".
 * [[Costar]] `F[_]` is also the [[Cokleisli]] category for `F[_]`.
 */
final case class Costar[F[_], B, A](runCostar: F[B] => A) { self =>
  def hoist[G[_]](f: G ~> F)(implicit ev: Profunctor[* => *]): Costar[G, B, A] = hoistCostar(f)(self)
}

abstract class CostarInstances {
  implicit final def composeCostar[F[_] : CoflatMap]: Compose[Costar[F, *, *]] = new Compose[Costar[F, *, *]] {
    override def compose[A, B, C](f: Costar[F, B, C], g: Costar[F, A, B]): Costar[F, A, C] =
      Costar(Cokleisli(f.runCostar).compose(Cokleisli(g.runCostar)).run)
  }

  implicit final def categoryCostar[F[_]](implicit ev: Comonad[F]): Category[Costar[F, *, *]] = new Category[Costar[F, *, *]] {
    override def id[A]: Costar[F, A, A] = Costar(ev.extract)

    override def compose[A, B, C](f: Costar[F, B, C], g: Costar[F, A, B]): Costar[F, A, C] =
      composeCostar[F].compose(f, g)
  }

  implicit final def functorCostar[F[_], C]: Functor[Costar[F, C, *]] = new Functor[Costar[F, C, *]] {
    override def map[A, B](fa: Costar[F, C, A])(f: A => B): Costar[F, C, B] = Costar(f compose fa.runCostar)
  }

  implicit final def invariantCostar[F[_], C]: Invariant[Costar[F, C, *]] = new Invariant[Costar[F, C, *]] {
    override def imap[A, B](fa: Costar[F, C, A])(f: A => B)(g: B => A): Costar[F, C, B] = Costar(f compose fa.runCostar)
  }

  implicit final def applyCostar[F[_], C]: Apply[Costar[F, C, *]] = new Apply[Costar[F, C, *]] {
    override def ap[A, B](ff: Costar[F, C, A => B])(faCostar: Costar[F, C, A]): Costar[F, C, B] =
      Costar(fa => ff.runCostar(fa)(faCostar.runCostar(fa)))

    override def map[A, B](fa: Costar[F, C, A])(f: A => B): Costar[F, C, B] = functorCostar.map(fa)(f)
  }

  implicit final def applicativeCostar[F[_], C]: Applicative[Costar[F, C, *]] = new Applicative[Costar[F, C, *]] {
    override def pure[A](x: A): Costar[F, C, A] = Costar(const(x))

    override def ap[A, B](ff: Costar[F, C, A => B])(fa: Costar[F, C, A]): Costar[F, C, B] = applicativeCostar.ap(ff)(fa)
  }

  implicit final def bindCostar[F[_], C]: FlatMap[Costar[F, C, *]] = new FlatMap[Costar[F, C, *]] {
    override def flatMap[A, B](fa: Costar[F, C, A])(f: A => Costar[F, C, B]): Costar[F, C, B] =
      Costar(fc => f(fa.runCostar(fc)).runCostar(fc))

    override def tailRecM[A, B](a: A)(f: A => Costar[F, C, Either[A, B]]): Costar[F, C, B] =
      Costar(fc => f(a).runCostar(fc) match {
        case Left(a) => tailRecM(a)(f).runCostar(fc)
        case Right(value) => value
      })

    override def map[A, B](fa: Costar[F, C, A])(f: A => B): Costar[F, C, B] = functorCostar.map(fa)(f)
  }

  implicit final def monadCostar[F[_], C]: Monad[Costar[F, C, *]] = new Monad[Costar[F, C, *]] {
    override def flatMap[A, B](fa: Costar[F, C, A])(f: A => Costar[F, C, B]): Costar[F, C, B] = bindCostar.flatMap(fa)(f)

    override def tailRecM[A, B](a: A)(f: A => Costar[F, C, Either[A, B]]): Costar[F, C, B] = bindCostar.tailRecM(a)(f)

    override def pure[A](x: A): Costar[F, C, A] = applicativeCostar.pure(x)
  }

  implicit final def distributiveCostar[F[A], C]: Distributive[Costar[F, C, *]] = new Distributive[Costar[F, C, *]] {
    override def distribute[G[_], A, B](ga: G[A])(f: A => Costar[F, C, B])(implicit ev: Functor[G]): Costar[F, C, G[B]] =
      Costar(fc => ev.map(ga)(a => f(a).runCostar(fc)))

    override def map[A, B](fa: Costar[F, C, A])(f: A => B): Costar[F, C, B] = functorCostar.map(fa)(f)
  }

  implicit final def profunctorCostar[F[_]](implicit ev: Functor[F]): Profunctor[Costar[F, *, *]] = new Profunctor[Costar[F, *, *]] {
    override def dimap[A, B, C, D](fab: Costar[F, A, B])(f: C => A)(g: B => D): Costar[F, C, D] =
      Costar(g compose fab.runCostar compose ev.lift(f))
  }

  implicit final def strongCostar[F[_]](implicit ev: Comonad[F]): Strong[Costar[F, *, *]] = new Strong[Costar[F, *, *]] {
    override def first[A, B, C](costar: Costar[F, A, B]): Costar[F, (A, C), (B, C)] = Costar(fac => {
      val fa = ev.map(fac)(_._1)
      val fbc = ev.map(fac) { case (_, c) => (costar.runCostar(fa), c) }

      ev.extract(fbc)
    })

    override def second[A, B, C](costar: Costar[F, A, B]): Costar[F, (C, A), (C, B)] = Costar(fca => {
      val fa = ev.map(fca)(_._2)
      val fcb = ev.map(fca) { case (c, _) => (c, costar.runCostar(fa)) }

      ev.extract(fcb)
    })

    override def dimap[A, B, C, D](fab: Costar[F, A, B])(f: C => A)(g: B => D): Costar[F, C, D] =
      profunctorCostar[F].dimap(fab)(f)(g)
  }

  implicit final def costrongCostar[F[_]](implicit ev: Functor[F]): Costrong[Costar[F, *, *]] = new Costrong[Costar[F, *, *]] {
    override def unfirst[A, B, C](p: Costar[F, (A, C), (B, C)]): Costar[F, A, B] = Costar(fa => {
      lazy val bd: (B, C) = p.runCostar(ev.map(fa)(a => (a, bd._2)))
      bd._1
    })

    override def unsecond[A, B, C](p: Costar[F, (A, B), (A, C)]): Costar[F, B, C] = Costar(fb => {
      lazy val db: (A, C) = p.runCostar(ev.map(fb)(b => (db._1, b)))
      db._2
    })

    override def dimap[A, B, C, D](fab: Costar[F, A, B])(f: C => A)(g: B => D): Costar[F, C, D] =
      profunctorCostar[F].dimap(fab)(f)(g)
  }

  implicit final def choiceCostar[F[_]](implicit ev0: Applicative[F], ev1: Comonad[F]): Choice[Costar[F, *, *]] = new Choice[Costar[F, *, *]] {
    override def left[A, B, C](pab: Costar[F, A, B]): Costar[F, Either[A, C], Either[B, C]] =
      Costar(ev1.extract[Either[B, C]] _ compose ev0.lift(_.bimap(pab.runCostar compose ev0.pure[A], identity[C])))

    override def right[A, B, C](pab: Costar[F, B, C]): Costar[F, Either[A, B], Either[A, C]] =
      Costar(ev1.extract[Either[A, C]] _ compose ev0.lift(_.bimap(identity[A], pab.runCostar compose ev0.pure[B])))

    override def dimap[A, B, C, D](fab: Costar[F, A, B])(f: C => A)(g: B => D): Costar[F, C, D] =
      profunctorCostar[F](ev0).dimap(fab)(f)(g)
  }

  implicit final def cochoiceCostar[F[_]](implicit ev: Applicative[F]): Cochoice[Costar[F, *, *]] = new Cochoice[Costar[F, *, *]] {
    override def unleft[A, B, C](p: Costar[F, Either[A, C], Either[B, C]]): Costar[F, A, B] = {
      def g(e1: F[Either[A, C]]): B = {
        def f(e2: Either[B, C]): B = {
          e2.fold(identity[B], c => g(ev.pure(c.asRight[A])))
        }

        f(p.runCostar(e1))
      }

      Costar(g _ compose ev.lift(Left[A, C]))
    }

    override def unright[A, B, C](p: Costar[F, Either[A, B], Either[A, C]]): Costar[F, B, C] = {
      def g(e1: F[Either[A, B]]): C = {
        def f(e2: Either[A, C]): C = {
          e2.fold(a => g(ev.pure(a.asLeft[B])), identity[C])
        }

        f(p.runCostar(e1))
      }

      Costar(g _ compose ev.lift(Right[A, B]))
    }

    override def dimap[A, B, C, D](fab: Costar[F, A, B])(f: C => A)(g: B => D): Costar[F, C, D] =
      profunctorCostar[F].dimap(fab)(f)(g)
  }

  implicit final def closedCostar[F[_]](implicit ev: Functor[F]): Closed[Costar[F, *, *]] = new Closed[Costar[F, *, *]] {
    override def closed[A, B, C](pab: Costar[F, A, B]): Costar[F, C => A, C => B] =
      Costar[F, C => A, C => B](fca => c => pab.runCostar(ev.map(fca)(_.apply(c))))

    override def dimap[A, B, C, D](fab: Costar[F, A, B])(f: C => A)(g: B => D): Costar[F, C, D] =
      profunctorCostar[F].dimap(fab)(f)(g)
  }
}

object Costar extends CostarInstances {
  def hoistCostar[F[_], G[_], A, B](f: G ~> F)(coStar: Costar[F, A, B])(implicit ev: Profunctor[* => *]): Costar[G, A, B] =
    Costar[G, A, B](ev.lmap(coStar.runCostar)(f.apply[A]))
}

