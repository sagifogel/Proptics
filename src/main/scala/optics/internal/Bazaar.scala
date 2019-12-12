package optics.internal

import cats.{Applicative, Monad}
import cats.arrow.{ArrowChoice, Profunctor, Strong}

import scala.Function.const

/** [[Bazaar]] is used to characterize a Traversal */
final case class Bazaar[P[_, _], F[_]: Applicative, A, B, S, T](runBazaar: P[A, F[B]] => S => F[T])

abstract class BazaarInstances {
  implicit def profunctorBazaar[P[_, _], F[_], G, H](implicit ev: Applicative[F]): Profunctor[Bazaar[P, F, G, H, *, *]] =
    new Profunctor[Bazaar[P, F, G, H, *, *]] {
      override def dimap[A, B, C, D](fab: Bazaar[P, F, G, H, A, B])(f: C => A)(g: B => D): Bazaar[P, F, G, H, C, D] = {
        Bazaar(pfgh => c => {
          val fb = fab.runBazaar(pfgh)(f(c))
          ev.map(fb)(g)
        })
      }
    }

  implicit def strongBazaar[P[_, _], F[_], G, H](implicit ev: Applicative[F]): Strong[Bazaar[P, F, G, H, *, *]] =
    new Strong[Bazaar[P, F, G, H, *, *]] {
      override def first[A, B, C](fa: Bazaar[P, F, G, H, A, B]): Bazaar[P, F, G, H, (A, C), (B, C)] = {
        Bazaar(pgfh => {
          case (a, c) =>
            val afb = fa.runBazaar(pgfh)
            ev.map(afb(a))((_, c))
        })
      }

      override def second[A, B, C](fa: Bazaar[P, F, G, H, A, B]): Bazaar[P, F, G, H, (C, A), (C, B)] =
        Bazaar(pgfh => {
          case (c, a) =>
            val afb = fa.runBazaar(pgfh)
            ev.map(afb(a))((c, _))
        })

      override def dimap[A, B, C, D](fab: Bazaar[P, F, G, H, A, B])(f: C => A)(g: B => D): Bazaar[P, F, G, H, C, D] =
        profunctorBazaar[P, F, G, H].dimap(fab)(f)(g)
    }

  implicit def choiceBazaar[P[_, _], F[_], G, H](implicit ev: Monad[F]): ArrowChoice[Bazaar[P, F, G, H, *, *]] =
    new ArrowChoice[Bazaar[P, F, G, H, *, *]] {
      override def choose[A, B, C, D](f: Bazaar[P, F, G, H, A, C])(g: Bazaar[P, F, G, H, B, D]):
        Bazaar[P, F, G, H, Either[A, B], Either[C, D]] = ???

      override def lift[A, B](f: A => B): Bazaar[P, F, G, H, A, B] =
        Bazaar[P, F, G, H, A, B](const(ev.pure[B] _ compose f))

      override def compose[A, B, C](f: Bazaar[P, F, G, H, B, C], g: Bazaar[P, F, G, H, A, B]):
        Bazaar[P, F, G, H, A, C] = Bazaar[P, F, G, H, A, C] { pgfh =>
          a: A => {
            val fb = g.runBazaar(pgfh)(a)
            ev.flatMap(fb)(f.runBazaar(pgfh))
          }
      }

      override def first[A, B, C](fa: Bazaar[P, F, G, H, A, B]): Bazaar[P, F, G, H, (A, C), (B, C)] = ???
    }
}