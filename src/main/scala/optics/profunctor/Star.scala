package optics.profunctor

import cats.arrow.Category
import cats.syntax.flatMap._
import cats.syntax.apply._
import cats.syntax.semigroupk._
import cats.{Alternative, Applicative, Apply, CommutativeMonad, Distributive, FlatMap, Functor, Invariant, Monad, MonoidK}

import scala.Function.const

final case class Star[F[_], A, B](runStar: A => F[B])

abstract class StarInstances {
  implicit def categoryStar[F[_]](implicit ev: Monad[F]): Category[Star[F, *, *]] = new Category[Star[F, *, *]] {
    override def id[A]: Star[F, A, A] = Star(ev.pure)

    override def compose[A, B, C](f: Star[F, B, C], g: Star[F, A, B]): Star[F, A, C] =
      Star[F, A, C](g.runStar(_) >>= f.runStar)
  }

  implicit def functorStar[F[_], C](implicit ev: Functor[F]): Functor[Star[F, C, *]] = new Functor[Star[F, C, *]] {
    override def map[A, B](fa: Star[F, C, A])(f: A => B): Star[F, C, B] = {
      Star[F, C, B](ev.lift(f) compose fa.runStar)
    }
  }

  implicit def invariantStar[F[_], E](implicit ev: Invariant[F]): Invariant[Star[F, E, *]] = new Invariant[Star[F, E, *]] {
    override def imap[A, B](fa: Star[F, E, A])(f: A => B)(g: B => A): Star[F, E, B] = {
      val imap: F[A] => F[B] = ev.imap(_)(f)(g)
      Star(imap compose fa.runStar)
    }
  }

  implicit def applyStar[F[_], E](implicit ev: Apply[F], ev2: Functor[F]): Apply[Star[F, E, *]] = new Apply[Star[F, E, *]] {
    override def ap[A, B](ff: Star[F, E, A => B])(fa: Star[F, E, A]): Star[F, E, B] =
      Star(e => ff.runStar(e) <*> fa.runStar(e))

    override def map[A, B](fa: Star[F, E, A])(f: A => B): Star[F, E, B] =
      functorStar[F, E].map(fa)(f)
  }

  implicit def applicativeStar[F[_], E](implicit ev: Applicative[F]): Applicative[Star[F, E, *]] =
    new Applicative[Star[F, E, *]] {
      override def pure[A](x: A): Star[F, E, A] = Star(const(ev.pure(x)))

      override def ap[A, B](ff: Star[F, E, A => B])(fa: Star[F, E, A]): Star[F, E, B] =
        applyStar[F, E].ap(ff)(fa)
    }

  implicit def bindStar[F[_], E](implicit ev: FlatMap[F], ev2: Applicative[F]): FlatMap[Star[F, E, *]] = new FlatMap[Star[F, E, *]] {
    override def flatMap[A, B](fa: Star[F, E, A])(f: A => Star[F, E, B]): Star[F, E, B] =
      Star(e => {
        fa.runStar(e) >>= (a => f(a).runStar(e))
      })

    override def tailRecM[A, B](a: A)(f: A => Star[F, E, Either[A, B]]): Star[F, E, B] = {
      Star(e => ev.flatMap(f(a).runStar(e)) {
        case Left(aa) => tailRecM(aa)(f).runStar(e)
        case Right(b) => ev2.pure(b)
      })
    }

    override def map[A, B](fa: Star[F, E, A])(f: A => B): Star[F, E, B] =
      functorStar[F, E](ev).map(fa)(f)
  }

  implicit def monadStar[F[_], E](implicit ev: Monad[F]): Monad[Star[F, E, *]] = new Monad[Star[F, E, *]] {
    override def pure[A](x: A): Star[F, E, A] = applicativeStar[F, E].pure(x)

    override def flatMap[A, B](fa: Star[F, E, A])(f: A => Star[F, E, B]): Star[F, E, B] =
      bindStar[F, E].flatMap(fa)(f)

    override def tailRecM[A, B](a: A)(f: A => Star[F, E, Either[A, B]]): Star[F, E, B] =
      bindStar[F, E].tailRecM(a)(f)
  }

  def altStar[F[_], E](implicit ev: Alternative[F]): Alternative[Star[F, E, *]] = new Alternative[Star[F, E, *]] {
    override def pure[A](x: A): Star[F, E, A] = applicativeStar[F, E].pure(x)

    override def ap[A, B](ff: Star[F, E, A => B])(fa: Star[F, E, A]): Star[F, E, B] =
      applicativeStar[F, E].ap(ff)(fa)

    override def empty[A]: Star[F, E, A] = plusStar[F, E].empty

    override def combineK[A](x: Star[F, E, A], y: Star[F, E, A]): Star[F, E, A] =
      plusStar[F, E].combineK(x, y)
  }

  def plusStar[F[_], E](implicit ev: MonoidK[F]): MonoidK[Star[F, E, *]] = new MonoidK[Star[F, E, *]] {
    override def empty[A]: Star[F, E, A] = Star(const(ev.empty))

    override def combineK[A](x: Star[F, E, A], y: Star[F, E, A]): Star[F, E, A] =
      Star(e => x.runStar(e) <+> y.runStar(e))
  }

  def monadZeroStar[F[_], E](implicit ev: CommutativeMonad[F]): CommutativeMonad[Star[F, E, *]] =
    new CommutativeMonad[Star[F, E, *]] {
      override def pure[A](x: A): Star[F, E, A] = monadStar[F, E].pure(x)

      override def flatMap[A, B](fa: Star[F, E, A])(f: A => Star[F, E, B]): Star[F, E, B] =
        monadStar[F, E].flatMap(fa)(f)

      override def tailRecM[A, B](a: A)(f: A => Star[F, E, Either[A, B]]): Star[F, E, B] =
        monadStar[F, E].tailRecM(a)(f)
    }

  def distributiveStar[F[_], E](implicit ev: Distributive[F]): Distributive[Star[F, E, *]] =
    new Distributive[Star[F, E, *]] {
      override def distribute[G[_], A, B](ga: G[A])
                                         (f: A => Star[F, E, B])
                                         (implicit evidence$1: Functor[G]): Star[F, E, G[B]] =
        Star(e => ev.distribute(ga)(f(_).runStar(e)))

      override def map[A, B](fa: Star[F, E, A])(f: A => B): Star[F, E, B] =
        functorStar[F, E].map(fa)(f)
    }
}

object Star extends StarInstances