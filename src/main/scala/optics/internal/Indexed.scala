package optics.internal

import cats.syntax.either._
import cats.arrow.{ArrowChoice, Profunctor, Strong}

/** Profunctor used for [[optics.IndexedOptic]]'s. */
final case class Indexed[P[_, _], I, S, T](runIndex: P[(I, S), T])

abstract class IndexedInstances {
  implicit def profunctorIndexed[P[_, _], I](implicit ev: Profunctor[P]): Profunctor[Indexed[P, I, *, *]] =
    new Profunctor[Indexed[P, I, *, *]] {
      override def dimap[A, B, C, D](fab: Indexed[P, I, A, B])(f: C => A)(g: B => D): Indexed[P, I, C, D] = {
        Indexed(ev.dimap[(I, A), B, (I, C), D](fab.runIndex) { case (i, c) => (i, f(c)) }(g))
      }
    }

  implicit def strongIndexed[P[_, _], I](implicit ev: Strong[P]): Strong[Indexed[P, I, *, *]] =
    new Strong[Indexed[P, I, *, *]] {
      override def first[A, B, C](fa: Indexed[P, I, A, B]): Indexed[P, I, (A, C), (B, C)] = {
        val first: P[((I, A), C), (B, C)] = ev.first(fa.runIndex)
        Indexed(ev.lmap[((I, A), C), (B, C), (I, (A, C))](first) { case (i, (a, c)) => ((i, a), c) })
      }

      override def second[A, B, C](fa: Indexed[P, I, A, B]): Indexed[P, I, (C, A), (C, B)] = {
        val second: P[(C, (I, A)), (C, B)] = ev.second(fa.runIndex)
        Indexed(ev.lmap[(C, (I, A)), (C, B), (I, (C, A))](second) { case (c, (i, a)) => (i, (c, a)) })
      }

      override def dimap[A, B, C, D](fab: Indexed[P, I, A, B])(f: C => A)(g: B => D): Indexed[P, I, C, D] =
        profunctorIndexed[P, I].dimap(fab)(f)(g)
    }

  implicit def choiceIndexed[P[_, _], I](implicit ev: ArrowChoice[P]): ArrowChoice[Indexed[P, I, *, *]] =
    new ArrowChoice[Indexed[P, I, *, *]] {
      override def choose[A, B, C, D](f: Indexed[P, I, A, C])(g: Indexed[P, I, B, D]):
      Indexed[P, I, Either[A, B], Either[C, D]] = {
        val choose: P[Either[(I, A), (I, B)], Either[C, D]] = ev.choose(f.runIndex)(g.runIndex)

        Indexed(ev.lmap(choose) {
          case (i, ac) => ac.fold(a => (i, a).asLeft[(I, B)], b => (i, b).asRight[(I, A)])
        })
      }

      override def lift[A, B](f: A => B): Indexed[P, I, A, B] =
        Indexed(ev.lift[(I, A), B] { case (_, a) => f(a) })

      override def first[A, B, C](fa: Indexed[P, I, A, B]): Indexed[P, I, (A, C), (B, C)] = {
        strongIndexed[P, I].first(fa)
      }

      override def second[A, B, C](fa: Indexed[P, I, A, B]): Indexed[P, I, (C, A), (C, B)] = {
        strongIndexed[P, I].second(fa)
      }

      override def compose[A, B, C](f: Indexed[P, I, B, C], g: Indexed[P, I, A, B]): Indexed[P, I, A, C] = {
        ???
      }
    }
}

object Indexed extends IndexedInstances