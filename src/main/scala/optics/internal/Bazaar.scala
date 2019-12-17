package optics.internal

import cats.{Applicative, Bitraverse}
import cats.arrow.{Profunctor, Strong}
import cats.instances.either.catsStdBitraverseForEither
import cats.syntax.either._
import optics.profunctor.Choice

/** [[Bazaar]] is used to characterize a Traversal */
abstract class Bazaar[P[_, _], A, B, S, T] {
  def runBazaar[F[_]](implicit ev: Applicative[F]): RunBazaar[P, F, A, B, S, T]
}

abstract class RunBazaar[P[_, _], F[_] : Applicative, A, B, S, T] {
  def apply(f: P[A, F[B]]): S => F[T]
}

object RunBazaar {
  def apply[P[_, _], F[_] : Applicative, A, B, S, T](f: P[A, F[B]] => S => F[T]): RunBazaar[P, F, A, B, S, T] =
    new RunBazaar[P, F, A, B, S, T] {
      override def apply(pafb: P[A, F[B]]): S => F[T] = f(pafb)
    }
}

abstract class BazaarInstances {
  implicit def profunctorBazaar[P[_, _], G, H]: Profunctor[Bazaar[P, G, H, *, *]] = {
    new Profunctor[Bazaar[P, G, H, *, *]] {
      override def dimap[A, B, C, D](fab: Bazaar[P, G, H, A, B])(f: C => A)(g: B => D): Bazaar[P, G, H, C, D] =
        new Bazaar[P, G, H, C, D] {
          override def runBazaar[F[_]](implicit ev: Applicative[F]): RunBazaar[P, F, G, H, C, D] =
            RunBazaar(pgfh => c => {
              val run = fab.runBazaar
              val fb = run(pgfh)(f(c))

              ev.map(fb)(g)
            })
        }
    }
  }

  implicit def strongBazaar[P[_, _], G, H]: Strong[Bazaar[P, G, H, *, *]] = new Strong[Bazaar[P, G, H, *, *]] {
    override def first[A, B, C](fa: Bazaar[P, G, H, A, B]): Bazaar[P, G, H, (A, C), (B, C)] =
      new Bazaar[P, G, H, (A, C), (B, C)] {
        override def runBazaar[F[_]](implicit ev: Applicative[F]): RunBazaar[P, F, G, H, (A, C), (B, C)] =
          RunBazaar(pgfh => {
            case (a, c) =>
              val run = fa.runBazaar
              val afb = run(pgfh)
              ev.map(afb(a))((_, c))
          })
      }

    override def second[A, B, C](fa: Bazaar[P, G, H, A, B]): Bazaar[P, G, H, (C, A), (C, B)] =
      new Bazaar[P, G, H, (C, A), (C, B)] {
        override def runBazaar[F[_]](implicit ev: Applicative[F]): RunBazaar[P, F, G, H, (C, A), (C, B)] =
          RunBazaar(pgfh => {
            case (c, a) =>
              val run = fa.runBazaar
              val afb = run(pgfh)

              ev.map(afb(a))((c, _))
          })
      }

    override def dimap[A, B, C, D](fab: Bazaar[P, G, H, A, B])(f: C => A)(g: B => D): Bazaar[P, G, H, C, D] =
      profunctorBazaar[P, G, H].dimap(fab)(f)(g)
  }

  implicit def choiceBazaar[P[_, _], G, H]: Choice[Bazaar[P, G, H, *, *]] = new Choice[Bazaar[P, G, H, *, *]] {
    override def left[A, B, C](pab: Bazaar[P, G, H, A, B]): Bazaar[P, G, H, Either[A, C], Either[B, C]] =
      new Bazaar[P, G, H, Either[A, C], Either[B, C]] {
        override def runBazaar[F[_]](implicit ev: Applicative[F]): RunBazaar[P, F, G, H, Either[A, C], Either[B, C]] = {
          RunBazaar(pgfh => either => {
            val B = implicitly[Bitraverse[Either]]
            val run = pab.runBazaar
            B.bitraverse(either)(run(pgfh), ev.pure)
          })
        }
      }

    override def right[A, B, C](pab: Bazaar[P, G, H, B, C]): Bazaar[P, G, H, Either[A, B], Either[A, C]] =
      new Bazaar[P, G, H, Either[A, B], Either[A, C]] {
        override def runBazaar[F[_]](implicit ev: Applicative[F]): RunBazaar[P, F, G, H, Either[A, B], Either[A, C]] =
          RunBazaar(pgfh => either => {
            val run = pab.runBazaar
            either.traverse(run(pgfh))
          })
      }

    override def dimap[A, B, C, D](fab: Bazaar[P, G, H, A, B])(f: C => A)(g: B => D): Bazaar[P, G, H, C, D] =
      profunctorBazaar[P, G, H].dimap(fab)(f)(g)
  }

  implicit def wanderBazaar[P[_, _], G, H]: Wander[Bazaar[P, G, H, *, *]] = new Wander[Bazaar[P, G, H, *, *]] {
    override def wander[S, T, A, B](traversal: Traversal[S, T, A, B])(pab: Bazaar[P, G, H, A, B]):
    Bazaar[P, G, H, S, T] = new Bazaar[P, G, H, S, T] {
      override def runBazaar[F[_]](implicit ev: Applicative[F]): RunBazaar[P, F, G, H, S, T] =
        RunBazaar(pgfh => s => {
          val run = pab.runBazaar
          val sft = traversal(run(pgfh))
          sft(s)
        })
    }

    override def left[A, B, C](pab: Bazaar[P, G, H, A, B]): Bazaar[P, G, H, Either[A, C], Either[B, C]] =
      choiceBazaar[P, G, H].left(pab)

    override def right[A, B, C](pab: Bazaar[P, G, H, B, C]): Bazaar[P, G, H, Either[A, B], Either[A, C]] =
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