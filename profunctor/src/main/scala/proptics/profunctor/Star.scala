package proptics.profunctor

import cats.arrow.{Category, Profunctor, Strong}
import cats.syntax.apply._
import cats.syntax.flatMap._
import cats.syntax.semigroupk._
import cats.{Alternative, Applicative, Apply, CommutativeMonad, Distributive, FlatMap, Functor, Invariant, Monad, MonoidK}

import scala.Function.const

/**
  * [[Star]] turns a [[Functor]] into a [[Profunctor]] "forwards".
  * <p>
  * Star `F[_]` is also the [[cats.data.Kleisli]] category for `F[_]`.
  * </p>
  */
final case class Star[F[_], A, B](runStar: A => F[B]) extends AnyVal

abstract class StarInstances {
  implicit final def categoryStar[F[_]](implicit ev: Monad[F]): Category[Star[F, *, *]] = new Category[Star[F, *, *]] {
    override def id[A]: Star[F, A, A] = Star(ev.pure)

    override def compose[A, B, C](f: Star[F, B, C], g: Star[F, A, B]): Star[F, A, C] =
      Star[F, A, C](g.runStar(_) >>= f.runStar)
  }

  implicit final def functorStar[F[_], C](implicit ev: Functor[F]): Functor[Star[F, C, *]] = new Functor[Star[F, C, *]] {
    override def map[A, B](fa: Star[F, C, A])(f: A => B): Star[F, C, B] =
      Star[F, C, B](ev.lift(f) compose fa.runStar)
  }

  implicit final def invariantStar[F[_], E](implicit ev: Invariant[F]): Invariant[Star[F, E, *]] = new Invariant[Star[F, E, *]] {
    override def imap[A, B](fa: Star[F, E, A])(f: A => B)(g: B => A): Star[F, E, B] = {
      val imap: F[A] => F[B] = ev.imap(_)(f)(g)
      Star(imap compose fa.runStar)
    }
  }

  implicit final def applyStar[F[_], E](implicit ev0: Apply[F], ev1: Functor[F]): Apply[Star[F, E, *]] = new Apply[Star[F, E, *]] {
    override def ap[A, B](ff: Star[F, E, A => B])(fa: Star[F, E, A]): Star[F, E, B] =
      Star(e => ff.runStar(e) <*> fa.runStar(e))

    override def map[A, B](fa: Star[F, E, A])(f: A => B): Star[F, E, B] =
      functorStar[F, E].map(fa)(f)
  }

  implicit final def applicativeStar[F[_], E](implicit ev: Applicative[F]): Applicative[Star[F, E, *]] = new Applicative[Star[F, E, *]] {
    override def pure[A](x: A): Star[F, E, A] = Star(const(ev.pure(x)))

    override def ap[A, B](ff: Star[F, E, A => B])(fa: Star[F, E, A]): Star[F, E, B] =
      applyStar[F, E].ap(ff)(fa)
  }

  implicit final def bindStar[F[_], E](implicit ev0: FlatMap[F], ev1: Applicative[F]): FlatMap[Star[F, E, *]] = new FlatMap[Star[F, E, *]] {
    override def flatMap[A, B](fa: Star[F, E, A])(f: A => Star[F, E, B]): Star[F, E, B] =
      Star(e => fa.runStar(e) >>= (a => f(a).runStar(e)))

    override def tailRecM[A, B](a: A)(f: A => Star[F, E, Either[A, B]]): Star[F, E, B] =
      Star(e =>
        ev0.flatMap(f(a).runStar(e)) {
          case Left(aa) => tailRecM(aa)(f).runStar(e)
          case Right(b) => ev1.pure(b)
        })

    override def map[A, B](fa: Star[F, E, A])(f: A => B): Star[F, E, B] =
      functorStar[F, E](ev0).map(fa)(f)
  }

  implicit final def monadStar[F[_], E](implicit ev: Monad[F]): Monad[Star[F, E, *]] = new Monad[Star[F, E, *]] {
    override def pure[A](x: A): Star[F, E, A] = applicativeStar[F, E].pure(x)

    override def flatMap[A, B](fa: Star[F, E, A])(f: A => Star[F, E, B]): Star[F, E, B] =
      bindStar[F, E].flatMap(fa)(f)

    override def tailRecM[A, B](a: A)(f: A => Star[F, E, Either[A, B]]): Star[F, E, B] =
      bindStar[F, E].tailRecM(a)(f)
  }

  implicit final def alternativeStar[F[_], E](implicit ev: Alternative[F]): Alternative[Star[F, E, *]] = new Alternative[Star[F, E, *]] {
    override def pure[A](x: A): Star[F, E, A] = applicativeStar[F, E].pure(x)

    override def ap[A, B](ff: Star[F, E, A => B])(fa: Star[F, E, A]): Star[F, E, B] =
      applicativeStar[F, E].ap(ff)(fa)

    override def empty[A]: Star[F, E, A] = plusStar[F, E].empty

    override def combineK[A](x: Star[F, E, A], y: Star[F, E, A]): Star[F, E, A] =
      plusStar[F, E].combineK(x, y)
  }

  implicit final def plusStar[F[_], E](implicit ev: MonoidK[F]): MonoidK[Star[F, E, *]] = new MonoidK[Star[F, E, *]] {
    override def empty[A]: Star[F, E, A] = Star(const(ev.empty))

    override def combineK[A](x: Star[F, E, A], y: Star[F, E, A]): Star[F, E, A] =
      Star(e => x.runStar(e) <+> y.runStar(e))
  }

  implicit final def monadZeroStar[F[_], E](implicit ev: CommutativeMonad[F]): CommutativeMonad[Star[F, E, *]] = new CommutativeMonad[Star[F, E, *]] {
    override def pure[A](x: A): Star[F, E, A] = monadStar[F, E].pure(x)

    override def flatMap[A, B](fa: Star[F, E, A])(f: A => Star[F, E, B]): Star[F, E, B] =
      monadStar[F, E].flatMap(fa)(f)

    override def tailRecM[A, B](a: A)(f: A => Star[F, E, Either[A, B]]): Star[F, E, B] =
      monadStar[F, E].tailRecM(a)(f)
  }

  implicit final def distributiveStar[F[_], E](implicit ev0: Distributive[F]): Distributive[Star[F, E, *]] = new Distributive[Star[F, E, *]] {
    override def distribute[G[_], A, B](ga: G[A])(f: A => Star[F, E, B])(implicit ev1: Functor[G]): Star[F, E, G[B]] =
      Star(e => ev0.distribute(ga)(f(_).runStar(e)))

    override def map[A, B](fa: Star[F, E, A])(f: A => B): Star[F, E, B] =
      functorStar[F, E].map(fa)(f)
  }

  implicit final def profunctorStar[F[_]](implicit ev: Functor[F]): Profunctor[Star[F, *, *]] = new Profunctor[Star[F, *, *]] {
    override def dimap[A, B, C, D](fab: Star[F, A, B])(f: C => A)(g: B => D): Star[F, C, D] =
      Star(ev.lift(g) compose fab.runStar compose f)
  }

  implicit final def strongStar[F[_]](implicit ev: Functor[F]): Strong[Star[F, *, *]] = new Strong[Star[F, *, *]] {
    override def first[A, B, C](fa: Star[F, A, B]): Star[F, (A, C), (B, C)] =
      Star { case (a, c) => ev.map(fa.runStar(a))((_, c)) }

    override def second[A, B, C](fa: Star[F, A, B]): Star[F, (C, A), (C, B)] =
      Star { case (c, a) => ev.map(fa.runStar(a))((c, _)) }

    override def dimap[A, B, C, D](fab: Star[F, A, B])(f: C => A)(g: B => D): Star[F, C, D] =
      profunctorStar[F].dimap(fab)(f)(g)
  }
}

object Star extends StarInstances
