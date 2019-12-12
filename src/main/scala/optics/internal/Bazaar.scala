package optics.internal

import cats.Applicative
import cats.arrow.{Profunctor, Strong}

/** [[Bazaar]] is used to characterize a Traversal */
final case class Bazaar[P[_, _], F[_] : Applicative, A, B, S, T](x: P[A, F[B]] => S => F[T])

abstract class BazaarInstances {
  implicit def profunctorBazaar[P[_, _], F[_], G, H](implicit ev: Applicative[F]): Profunctor[Bazaar[P, F, G, H, *, *]] =
    new Profunctor[Bazaar[P, F, G, H, *, *]] {
      override def dimap[A, B, C, D](fab: Bazaar[P, F, G, H, A, B])(f: C => A)(g: B => D): Bazaar[P, F, G, H, C, D] = {
        Bazaar(pfgh => c => {
          val fb = fab.x(pfgh)(f(c))
          ev.map(fb)(g)
        })
      }
    }

  implicit def strongBazaar[P[_, _], F[_], G, H](implicit ev: Applicative[F]): Strong[Bazaar[P, F, G, H, *, *]] =
    new Strong[Bazaar[P, F, G, H, *, *]] {
      override def first[A, B, C](fa: Bazaar[P, F, G, H, A, B]): Bazaar[P, F, G, H, (A, C), (B, C)] = {
        Bazaar(pgfh => { case (a, c) =>
          val afb = fa.x(pgfh)
          ev.map(afb(a))((_, c))
        })
      }

      override def second[A, B, C](fa: Bazaar[P, F, G, H, A, B]): Bazaar[P, F, G, H, (C, A), (C, B)] =
        Bazaar(pgfh => { case (c, a) =>
          val afb = fa.x(pgfh)
          ev.map(afb(a))((c, _))
        })

      override def dimap[A, B, C, D](fab: Bazaar[P, F, G, H, A, B])(f: C => A)(g: B => D): Bazaar[P, F, G, H, C, D] =
        profunctorBazaar[P, F, G, H].dimap(fab)(f)(g)
    }
}

object Bazaar {
  def runBazaar[P[_, _], F[_] : Applicative, A, B, S, T](bazaar: Bazaar[P, F, A, B, S, T]): P[A, F[B]] => S => F[T] = {
    bazaar.x
  }
}