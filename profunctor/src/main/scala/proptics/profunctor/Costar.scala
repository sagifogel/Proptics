package proptics.profunctor

import scala.Function.const

import cats.arrow.{Category, Compose, Profunctor, Strong}
import cats.data.Cokleisli
import cats.syntax.either._
import cats.{Applicative, Apply, CoflatMap, Comonad, Distributive, FlatMap, Functor, Invariant, Monad, ~>}

abstract class CostarInstances {
  implicit final def composeCostar[F[_]: CoflatMap]: Compose[Costar[F, *, *]] = new Compose[Costar[F, *, *]] {
    override def compose[A, B, C](f: Costar[F, B, C], g: Costar[F, A, B]): Costar[F, A, C] =
      Costar(Cokleisli(f.run).compose(Cokleisli(g.run)).run)
  }

  implicit final def categoryCostar[F[_]](implicit ev: Comonad[F]): Category[Costar[F, *, *]] = new Category[Costar[F, *, *]] {
    override def id[A]: Costar[F, A, A] = Costar(ev.extract)

    override def compose[A, B, C](f: Costar[F, B, C], g: Costar[F, A, B]): Costar[F, A, C] =
      composeCostar[F].compose(f, g)
  }

  implicit final def functorCostar[F[_], C]: Functor[Costar[F, C, *]] = new Functor[Costar[F, C, *]] {
    override def map[A, B](fa: Costar[F, C, A])(f: A => B): Costar[F, C, B] = Costar(f compose fa.run)
  }

  implicit final def invariantCostar[F[_], C]: Invariant[Costar[F, C, *]] = new Invariant[Costar[F, C, *]] {
    override def imap[A, B](fa: Costar[F, C, A])(f: A => B)(g: B => A): Costar[F, C, B] = Costar(f compose fa.run)
  }

  implicit final def applyCostar[F[_], C]: Apply[Costar[F, C, *]] = new Apply[Costar[F, C, *]] {
    override def ap[A, B](ff: Costar[F, C, A => B])(faCostar: Costar[F, C, A]): Costar[F, C, B] =
      Costar(fa => ff.run(fa)(faCostar.run(fa)))

    override def map[A, B](fa: Costar[F, C, A])(f: A => B): Costar[F, C, B] = functorCostar.map(fa)(f)
  }

  implicit final def applicativeCostar[F[_], C]: Applicative[Costar[F, C, *]] = new Applicative[Costar[F, C, *]] {
    override def pure[A](x: A): Costar[F, C, A] = Costar(const(x))

    override def ap[A, B](ff: Costar[F, C, A => B])(fa: Costar[F, C, A]): Costar[F, C, B] = applicativeCostar.ap(ff)(fa)
  }

  implicit final def bindCostar[F[_], C]: FlatMap[Costar[F, C, *]] = new FlatMap[Costar[F, C, *]] {
    override def flatMap[A, B](fa: Costar[F, C, A])(f: A => Costar[F, C, B]): Costar[F, C, B] =
      Costar(fc => f(fa.run(fc)).run(fc))

    override def tailRecM[A, B](a: A)(f: A => Costar[F, C, Either[A, B]]): Costar[F, C, B] =
      Costar(fc =>
        f(a).run(fc) match {
          case Left(a)      => tailRecM(a)(f).run(fc)
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
      Costar(fc => ev.map(ga)(a => f(a).run(fc)))

    override def map[A, B](fa: Costar[F, C, A])(f: A => B): Costar[F, C, B] = functorCostar.map(fa)(f)
  }

  implicit final def profunctorCostar[F[_]](implicit ev: Functor[F]): Profunctor[Costar[F, *, *]] = new Profunctor[Costar[F, *, *]] {
    override def dimap[A, B, C, D](fab: Costar[F, A, B])(f: C => A)(g: B => D): Costar[F, C, D] =
      Costar(g compose fab.run compose ev.lift(f))
  }

  implicit final def strongCostar[F[_]](implicit ev: Comonad[F]): Strong[Costar[F, *, *]] = new Strong[Costar[F, *, *]] {
    override def first[A, B, C](costar: Costar[F, A, B]): Costar[F, (A, C), (B, C)] = Costar { fac =>
      val fa = ev.map(fac)(_._1)
      val fbc = ev.map(fac) { case (_, c) => (costar.run(fa), c) }

      ev.extract(fbc)
    }

    override def second[A, B, C](costar: Costar[F, A, B]): Costar[F, (C, A), (C, B)] = Costar { fca =>
      val fa = ev.map(fca)(_._2)
      val fcb = ev.map(fca) { case (c, _) => (c, costar.run(fa)) }

      ev.extract(fcb)
    }

    override def dimap[A, B, C, D](fab: Costar[F, A, B])(f: C => A)(g: B => D): Costar[F, C, D] =
      profunctorCostar[F].dimap(fab)(f)(g)
  }

  implicit final def cochoiceCostar[F[_]](implicit ev: Applicative[F]): Cochoice[Costar[F, *, *]] = new Cochoice[Costar[F, *, *]] {
    override def unleft[A, B, C](p: Costar[F, Either[A, C], Either[B, C]]): Costar[F, A, B] = {
      def g(e1: F[Either[A, C]]): B = {
        def f(e2: Either[B, C]): B =
          e2.fold(identity[B], c => g(ev.pure(c.asRight[A])))

        f(p.run(e1))
      }

      Costar(g _ compose ev.lift(Left[A, C]))
    }

    override def dimap[A, B, C, D](fab: Costar[F, A, B])(f: C => A)(g: B => D): Costar[F, C, D] =
      profunctorCostar[F].dimap(fab)(f)(g)

  }

  implicit final def closedCostar[F[_]](implicit ev: Functor[F]): Closed[Costar[F, *, *]] = new Closed[Costar[F, *, *]] {
    override def closed[A, B, C](pab: Costar[F, A, B]): Costar[F, C => A, C => B] =
      Costar[F, C => A, C => B](fca => c => pab.run(ev.map(fca)(_.apply(c))))

    override def dimap[A, B, C, D](fab: Costar[F, A, B])(f: C => A)(g: B => D): Costar[F, C, D] =
      profunctorCostar[F].dimap(fab)(f)(g)
  }
}

object Costar extends CostarInstances {
  def apply[F[_], A, B](f: F[A] => B): Costar[F, A, B] = Cokleisli[F, A, B](f)

  def hoistCostar[F[_], G[_], A, B](f: G ~> F)(costar: Costar[F, A, B]): Costar[G, A, B] =
    Costar[G, A, B](Profunctor[* => *].lmap(costar.run)(f.apply[A]))
}
