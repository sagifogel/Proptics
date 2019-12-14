package optics.internal

import cats.{Applicative, Monad}
import cats.arrow.{ArrowChoice, Profunctor, Strong}

import scala.Function.const

abstract class RunBazaar[P[_, _], F[_] : Applicative, A, B, S, T] {
  def apply(f: P[A, F[B]]): S => F[T]
}

object RunBazaar {
  def apply[P[_, _], F[_] : Applicative, A, B, S, T](f: P[A, F[B]] => S => F[T]): RunBazaar[P, F, A, B, S, T] =
    new RunBazaar[P, F, A, B, S, T] {
      override def apply(pafb: P[A, F[B]]): S => F[T] = f(pafb)
    }
}

abstract class Bazaar[P[_, _], A, B, S, T] {
  def runBazaar[F[_]](implicit ev: Monad[F]): RunBazaar[P, F, A, B, S, T]
}

abstract class Bazaar2Instances {
  implicit def profunctorBazaar[P[_, _], G, H]: Profunctor[Bazaar[P, G, H, *, *]] = {
    new Profunctor[Bazaar[P, G, H, *, *]] {
      override def dimap[A, B, C, D](fab: Bazaar[P, G, H, A, B])(f: C => A)(g: B => D): Bazaar[P, G, H, C, D] =
        new Bazaar[P, G, H, C, D] {
          override def runBazaar[F[_]](implicit ev: Monad[F]): RunBazaar[P, F, G, H, C, D] =
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
        override def runBazaar[F[_]](implicit ev: Monad[F]): RunBazaar[P, F, G, H, (A, C), (B, C)] =
          RunBazaar(pgfh => {
            case (a, c) =>
              val run = fa.runBazaar
              val afb = run(pgfh)
              ev.map(afb(a))((_, c))
          })
      }

    override def second[A, B, C](fa: Bazaar[P, G, H, A, B]): Bazaar[P, G, H, (C, A), (C, B)] =
      new Bazaar[P, G, H, (C, A), (C, B)] {
        override def runBazaar[F[_]](implicit ev: Monad[F]): RunBazaar[P, F, G, H, (C, A), (C, B)] =
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

  implicit def choiceBazaar[P[_, _], G, H]: ArrowChoice[Bazaar[P, G, H, *, *]] = new ArrowChoice[Bazaar[P, G, H, *, *]] {
    override def choose[A, B, C, D](f: Bazaar[P, G, H, A, C])(g: Bazaar[P, G, H, B, D]):
    Bazaar[P, G, H, Either[A, B], Either[C, D]] = new Bazaar[P, G, H, Either[A, B], Either[C, D]] {
      override def runBazaar[F[_]](implicit ev: Monad[F]): RunBazaar[P, F, G, H, Either[A, B], Either[C, D]] =
        RunBazaar[P, F, G, H, Either[A, B], Either[C, D]](pgfh => {
          case Left(a) =>
            val run = f.runBazaar
            val fc = run(pgfh)(a)
            ev.map(fc)(Left[C, D])
          case Right(b) =>
            val run = g.runBazaar
            val fd = run(pgfh)(b)
            ev.map(fd)(Right[C, D])
        })
    }

    override def lift[A, B](f: A => B): Bazaar[P, G, H, A, B] = new Bazaar[P, G, H, A, B] {
      override def runBazaar[F[_]](implicit ev: Monad[F]): RunBazaar[P, F, G, H, A, B] =
        RunBazaar(const(ev.pure[B] _ compose f))
    }

    override def compose[A, B, C](f: Bazaar[P, G, H, B, C], g: Bazaar[P, G, H, A, B]): Bazaar[P, G, H, A, C] =
      new Bazaar[P, G, H, A, C] {
        override def runBazaar[F[_]](implicit ev: Monad[F]): RunBazaar[P, F, G, H, A, C] =
          RunBazaar(pgfh => {
            a: A => {
              val runG = g.runBazaar
              val fb = runG(pgfh)(a)
              val runF = f.runBazaar

              ev.flatMap(fb)(runF(pgfh))
            }
          })
      }

    override def first[A, B, C](fa: Bazaar[P, G, H, A, B]): Bazaar[P, G, H, (A, C), (B, C)] =
      strongBazaar[P, G, H].first(fa)
  }

  implicit def wanderBazaar[P[_, _], G, H]: Wander[Bazaar[P, G, H, *, *]] = new Wander[Bazaar[P, G, H, *, *]] {
    override def wander[S, T, A, B](traversal: Traversal[S, T, A, B])
                                   (pab: Bazaar[P, G, H, A, B])
                                   (implicit ev: Strong[Bazaar[P, G, H, *, *]],
                                    ev2: ArrowChoice[Bazaar[P, G, H, *, *]]): Bazaar[P, G, H, S, T] =
      new Bazaar[P, G, H, S, T] {
        override def runBazaar[F[_]](implicit ev: Monad[F]): RunBazaar[P, F, G, H, S, T] =
          RunBazaar(pgfh => s => {
            val run = pab.runBazaar
            val sft = traversal(run(pgfh))
            sft(s)
          })
      }
  }
}