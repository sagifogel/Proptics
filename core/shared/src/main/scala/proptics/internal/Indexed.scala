package proptics.internal

import cats.Applicative
import cats.arrow.{Profunctor, Strong}
import cats.syntax.bifunctor._
import cats.syntax.either._

import proptics.profunctor.{Choice, Traversing, Wander}

/** [[cats.arrow.Profunctor]] used for indexed optics */
final case class Indexed[P[_, _], I, S, T](runIndex: P[(S, I), T]) {
  /** remap the index */
  def reindex[J](f: J => I)(implicit ev: Profunctor[P]): Indexed[P, J, S, T] =
    Indexed(ev.lmap[(S, I), T, (S, J)](runIndex) { case (s, j) => (s, f(j)) })
}

abstract class IndexedInstances {
  implicit final def profunctorIndexed[P[_, _], I](implicit ev: Profunctor[P]): Profunctor[Indexed[P, I, *, *]] = new Profunctor[Indexed[P, I, *, *]] {
    override def dimap[A, B, C, D](fab: Indexed[P, I, A, B])(f: C => A)(g: B => D): Indexed[P, I, C, D] =
      Indexed(ev.dimap[(A, I), B, (C, I), D](fab.runIndex)(_.leftMap(f))(g))
  }

  implicit final def strongIndexed[P[_, _], I](implicit ev: Strong[P]): Strong[Indexed[P, I, *, *]] = new Strong[Indexed[P, I, *, *]] {
    override def first[A, B, C](fa: Indexed[P, I, A, B]): Indexed[P, I, (A, C), (B, C)] = {
      val first: P[((A, I), C), (B, C)] = ev.first(fa.runIndex)
      Indexed(ev.lmap[((A, I), C), (B, C), ((A, C), I)](first) { case ((a, c), i) => ((a, i), c) })
    }

    override def second[A, B, C](fa: Indexed[P, I, A, B]): Indexed[P, I, (C, A), (C, B)] = {
      val second: P[(C, (A, I)), (C, B)] = ev.second(fa.runIndex)
      Indexed(ev.lmap[(C, (A, I)), (C, B), ((C, A), I)](second) { case ((c, a), i) => (c, (a, i)) })
    }

    override def dimap[A, B, C, D](fab: Indexed[P, I, A, B])(f: C => A)(g: B => D): Indexed[P, I, C, D] =
      profunctorIndexed[P, I].dimap(fab)(f)(g)
  }

  implicit final def choiceIndexed[P[_, _], I](implicit ev: Choice[P]): Choice[Indexed[P, I, *, *]] = new Choice[Indexed[P, I, *, *]] {
    override def left[A, B, C](pab: Indexed[P, I, A, B]): Indexed[P, I, Either[A, C], Either[B, C]] = {
      val left: P[Either[(A, I), C], Either[B, C]] = ev.left(pab.runIndex)

      Indexed(ev.lmap(left) { case (ac, i) =>
        ac.fold(a => (a, i).asLeft[C], _.asRight[(A, I)])
      })
    }

    override def right[A, B, C](pab: Indexed[P, I, A, B]): Indexed[P, I, Either[C, A], Either[C, B]] = {
      val right: P[Either[C, (A, I)], Either[C, B]] = ev.right(pab.runIndex)

      Indexed(ev.lmap(right) { case (ca, i) =>
        ca.fold(_.asLeft[(A, I)], a => (a, i).asRight[C])
      })
    }

    override def dimap[A, B, C, D](fab: Indexed[P, I, A, B])(f: C => A)(g: B => D): Indexed[P, I, C, D] =
      profunctorIndexed[P, I].dimap(fab)(f)(g)
  }

  implicit final def wanderIndexed[P[_, _], I](implicit ev: Wander[P]): Wander[Indexed[P, I, *, *]] = new Wander[Indexed[P, I, *, *]] {
    override def wander[S, T, A, B](traversing: Traversing[S, T, A, B])(indexed: Indexed[P, I, A, B]): Indexed[P, I, S, T] = {
      val traverse = new Traversing[(S, I), T, (A, I), B] {
        override def apply[F[_]](f: ((A, I)) => F[B])(s: (S, I))(implicit ev: Applicative[F]): F[T] =
          traversing(a => f((a, s._2)))(s._1)
      }

      Indexed(ev.wander(traverse)(indexed.runIndex))
    }

    override def first[A, B, C](fa: Indexed[P, I, A, B]): Indexed[P, I, (A, C), (B, C)] =
      strongIndexed[P, I].first(fa)

    override def second[A, B, C](fa: Indexed[P, I, A, B]): Indexed[P, I, (C, A), (C, B)] =
      strongIndexed[P, I].second(fa)

    override def left[A, B, C](pab: Indexed[P, I, A, B]): Indexed[P, I, Either[A, C], Either[B, C]] =
      choiceIndexed[P, I].left(pab)

    override def right[A, B, C](pab: Indexed[P, I, A, B]): Indexed[P, I, Either[C, A], Either[C, B]] =
      choiceIndexed[P, I].right(pab)

    override def dimap[A, B, C, D](fab: Indexed[P, I, A, B])(f: C => A)(g: B => D): Indexed[P, I, C, D] =
      profunctorIndexed[P, I].dimap(fab)(f)(g)
  }
}

object Indexed extends IndexedInstances
