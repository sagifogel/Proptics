package proptics.internal

import cats.{Applicative, Bitraverse}
import cats.arrow.{Profunctor, Strong}
import cats.instances.either._
import cats.syntax.either._
import proptics.profunctor.Choice
import proptics.rank2types.Traversing

/** Bazaar is used to characterize a [[proptics.Traversal_]] */
trait Bazaar[P[_, _], A, B, S, T] {
  def runBazaar: RunBazaar[P, A, B, S, T]
}

trait RunBazaar[P[_, _], A, B, S, T] {
  def apply[F[_]](pafb: P[A, F[B]])(s: S)(implicit ev: Applicative[F]): F[T]
}

abstract class BazaarInstances {
  implicit final def profunctorBazaar[P[_, _], G, H]: Profunctor[Bazaar[P, G, H, *, *]] = new Profunctor[Bazaar[P, G, H, *, *]] {
    override def dimap[A, B, C, D](fab: Bazaar[P, G, H, A, B])(f: C => A)(g: B => D): Bazaar[P, G, H, C, D] = new Bazaar[P, G, H, C, D] {
      override def runBazaar: RunBazaar[P, G, H, C, D] = new RunBazaar[P, G, H, C, D] {
        override def apply[F[_]](pafb: P[G, F[H]])(s: C)(implicit ev: Applicative[F]): F[D] =
          ev.map(fab.runBazaar(pafb)(f(s)))(g)
      }
    }
  }

  implicit final def strongBazaar[P[_, _], G, H]: Strong[Bazaar[P, G, H, *, *]] = new Strong[Bazaar[P, G, H, *, *]] {
    override def first[A, B, C](fa: Bazaar[P, G, H, A, B]): Bazaar[P, G, H, (A, C), (B, C)] = new Bazaar[P, G, H, (A, C), (B, C)] {
      override def runBazaar: RunBazaar[P, G, H, (A, C), (B, C)] = new RunBazaar[P, G, H, (A, C), (B, C)] {
        override def apply[F[_]](pafb: P[G, F[H]])(s: (A, C))(implicit ev: Applicative[F]): F[(B, C)] =
          ev.map(fa.runBazaar(pafb)(s._1))((_, s._2))
      }
    }

    override def second[A, B, C](fa: Bazaar[P, G, H, A, B]): Bazaar[P, G, H, (C, A), (C, B)] = new Bazaar[P, G, H, (C, A), (C, B)] {
      override def runBazaar: RunBazaar[P, G, H, (C, A), (C, B)] = new RunBazaar[P, G, H, (C, A), (C, B)] {
        override def apply[F[_]](pafb: P[G, F[H]])(s: (C, A))(implicit ev: Applicative[F]): F[(C, B)] =
          ev.map(fa.runBazaar(pafb)(s._2))((s._1, _))
      }
    }

    override def dimap[A, B, C, D](fab: Bazaar[P, G, H, A, B])(f: C => A)(g: B => D): Bazaar[P, G, H, C, D] =
      profunctorBazaar[P, G, H].dimap(fab)(f)(g)
  }

  implicit final def choiceBazaar[P[_, _], G, H]: Choice[Bazaar[P, G, H, *, *]] = new Choice[Bazaar[P, G, H, *, *]] {
    override def left[A, B, C](pab: Bazaar[P, G, H, A, B]): Bazaar[P, G, H, Either[A, C], Either[B, C]] = new Bazaar[P, G, H, Either[A, C], Either[B, C]] {
      override def runBazaar: RunBazaar[P, G, H, Either[A, C], Either[B, C]] = new RunBazaar[P, G, H, Either[A, C], Either[B, C]] {
        override def apply[F[_]](pafb: P[G, F[H]])(s: Either[A, C])(implicit ev: Applicative[F]): F[Either[B, C]] =
          Bitraverse[Either].bitraverse(s)(pab.runBazaar(pafb), ev.pure)
      }
    }

    override def right[A, B, C](pab: Bazaar[P, G, H, A, B]): Bazaar[P, G, H, Either[C, A], Either[C, B]] = new Bazaar[P, G, H, Either[C, A], Either[C, B]] {
      override def runBazaar: RunBazaar[P, G, H, Either[C, A], Either[C, B]] = new RunBazaar[P, G, H, Either[C, A], Either[C, B]] {
        override def apply[F[_]](pafb: P[G, F[H]])(s: Either[C, A])(implicit ev: Applicative[F]): F[Either[C, B]] =
          s.traverse(pab.runBazaar(pafb))
      }
    }

    override def dimap[A, B, C, D](fab: Bazaar[P, G, H, A, B])(f: C => A)(g: B => D): Bazaar[P, G, H, C, D] =
      profunctorBazaar[P, G, H].dimap(fab)(f)(g)
  }

  implicit final def wanderBazaar[P[_, _], G, H]: Wander[Bazaar[P, G, H, *, *]] = new Wander[Bazaar[P, G, H, *, *]] {
    override def wander[S, T, A, B](traversal: Traversing[S, T, A, B])(pab: Bazaar[P, G, H, A, B]): Bazaar[P, G, H, S, T] = new Bazaar[P, G, H, S, T] {
      override def runBazaar: RunBazaar[P, G, H, S, T] = new RunBazaar[P, G, H, S, T] {
        override def apply[F[_]](pafb: P[G, F[H]])(s: S)(implicit ev: Applicative[F]): F[T] =
          traversal(pab.runBazaar(pafb))(s)
      }
    }

    override def left[A, B, C](pab: Bazaar[P, G, H, A, B]): Bazaar[P, G, H, Either[A, C], Either[B, C]] =
      choiceBazaar[P, G, H].left(pab)

    override def right[A, B, C](pab: Bazaar[P, G, H, A, B]): Bazaar[P, G, H, Either[C, A], Either[C, B]] =
      choiceBazaar[P, G, H].right(pab)

    override def first[A, B, C](fa: Bazaar[P, G, H, A, B]): Bazaar[P, G, H, (A, C), (B, C)] =
      strongBazaar[P, G, H].first(fa)

    override def second[A, B, C](fa: Bazaar[P, G, H, A, B]): Bazaar[P, G, H, (C, A), (C, B)] =
      strongBazaar[P, G, H].second(fa)

    override def dimap[A, B, C, D](fab: Bazaar[P, G, H, A, B])(f: C => A)(g: B => D): Bazaar[P, G, H, C, D] =
      profunctorBazaar.dimap(fab)(f)(g)
  }
}

object Bazaar extends BazaarInstances
