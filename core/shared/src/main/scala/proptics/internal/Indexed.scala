package proptics.internal

import cats.Applicative
import cats.arrow.{Profunctor, Strong}
import cats.syntax.either._

import proptics.profunctor.{Choice, Traversing, Wander}

/** Profunctor used for indexed optics */
final case class Indexed[P[_, _], I, S, T](runIndex: P[(I, S), T]) extends AnyVal {
  /** remap the index */
  def reindex[J](f: ((J, S)) => (I, S))(implicit ev: Profunctor[P]): Indexed[P, J, S, T] =
    Indexed(ev.lmap[(I, S), T, (J, S)](runIndex)(f))
}

abstract class IndexedInstances {
  implicit final def profunctorIndexed[P[_, _], I](implicit ev: Profunctor[P]): Profunctor[Indexed[P, I, *, *]] = new Profunctor[Indexed[P, I, *, *]] {
    override def dimap[A, B, C, D](fab: Indexed[P, I, A, B])(f: C => A)(g: B => D): Indexed[P, I, C, D] =
      Indexed(ev.dimap[(I, A), B, (I, C), D](fab.runIndex) { case (i, c) => (i, f(c)) }(g))
  }

  implicit final def strongIndexed[P[_, _], I](implicit ev: Strong[P]): Strong[Indexed[P, I, *, *]] = new Strong[Indexed[P, I, *, *]] {
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

  implicit final def choiceIndexed[P[_, _], I](implicit ev: Choice[P]): Choice[Indexed[P, I, *, *]] = new Choice[Indexed[P, I, *, *]] {
    override def left[A, B, C](pab: Indexed[P, I, A, B]): Indexed[P, I, Either[A, C], Either[B, C]] = {
      val left: P[Either[(I, A), C], Either[B, C]] = ev.left(pab.runIndex)

      Indexed(ev.lmap(left) { case (i, ac) =>
        ac.fold(a => (i, a).asLeft[C], _.asRight[(I, A)])
      })
    }

    override def right[A, B, C](pab: Indexed[P, I, A, B]): Indexed[P, I, Either[C, A], Either[C, B]] = {
      val right: P[Either[C, (I, A)], Either[C, B]] = ev.right(pab.runIndex)

      Indexed(ev.lmap(right) { case (i, ca) =>
        ca.fold(_.asLeft[(I, A)], a => (i, a).asRight[C])
      })
    }

    override def dimap[A, B, C, D](fab: Indexed[P, I, A, B])(f: C => A)(g: B => D): Indexed[P, I, C, D] =
      profunctorIndexed[P, I].dimap(fab)(f)(g)
  }

  implicit final def wanderIndexed[P[_, _], I](implicit ev: Wander[P]): Wander[Indexed[P, I, *, *]] = new Wander[Indexed[P, I, *, *]] {
    override def wander[S, T, A, B](traversing: Traversing[S, T, A, B])(indexed: Indexed[P, I, A, B]): Indexed[P, I, S, T] = {
      val traversal = new Traversing[(I, S), T, (I, A), B] {
        override def apply[F[_]](f: ((I, A)) => F[B])(s: (I, S))(implicit ev: Applicative[F]): F[T] = {
          val fab = f compose (Tuple2.apply[I, A] _ curried)(s._1)

          traversing(fab)(s._2)
        }
      }

      Indexed(ev.wander(traversal)(indexed.runIndex))
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
