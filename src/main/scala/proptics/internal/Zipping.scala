package proptics.internal

import cats.{Id, SemigroupK}
import cats.instances.either._
import cats.syntax.either._
import cats.arrow.{Profunctor, Strong}
import proptics.profunctor.{Choice, Closed}
import scala.Function.const

final case class Zipping[A, B](runZipping: A => A => B)

abstract class ZippingInstances {
  implicit final val profunctorZipping: Profunctor[Zipping] = new Profunctor[Zipping] {
    override def dimap[A, B, C, D](fab: Zipping[A, B])(f: C => A)(g: B => D): Zipping[C, D] =
      Zipping[C, D](c1 => c2 => g(fab.runZipping(f(c1))(f(c2))))
  }

  implicit final val strongZipping: Strong[Zipping] = new Strong[Zipping] {
    override def first[A, B, C](fa: Zipping[A, B]): Zipping[(A, C), (B, C)] =
      Zipping(ac => ac2 => (fa.runZipping(ac._1)(ac2._1), ac2._2))

    override def second[A, B, C](fa: Zipping[A, B]): Zipping[(C, A), (C, B)] =
      Zipping(ca => ca2 => (ca2._1, fa.runZipping(ca._2)(ca2._2)))

    override def dimap[A, B, C, D](fab: Zipping[A, B])(f: C => A)(g: B => D): Zipping[C, D] =
      profunctorZipping.dimap(fab)(f)(g)
  }

  implicit final def choiceZipping: Choice[Zipping] = new Choice[Zipping] {
    override def left[A, B, C](pab: Zipping[A, B]): Zipping[Either[A, C], Either[B, C]] =
      Zipping(left => right =>
        SemigroupK[Either[A, *]].combineK(left, right)
          .fold(a => pab.runZipping(a)(a).asLeft[C], _.asRight[B]))

    override def right[A, B, C](pab: Zipping[B, C]): Zipping[Either[A, B], Either[A, C]] =
      Zipping(left => right =>
        SemigroupK[Either[A, *]].combineK(left, right)
          .fold(_.asLeft[C], b => pab.runZipping(b)(b).asRight[A]))

    override def dimap[A, B, C, D](fab: Zipping[A, B])(f: C => A)(g: B => D): Zipping[C, D] =
      profunctorZipping.dimap(fab)(f)(g)
  }

  implicit final val wanderZipping: Wander[Zipping] = new Wander[Zipping] {
    override def wander[S, T, A, B](traversal: Traversing[S, T, A, B])(pab: Zipping[A, B]): Zipping[S, T] =
      Zipping(const(traversal[Id](a => pab.runZipping(a)(a))))

    override def left[A, B, C](pab: Zipping[A, B]): Zipping[Either[A, C], Either[B, C]] = choiceZipping.left(pab)

    override def right[A, B, C](pab: Zipping[B, C]): Zipping[Either[A, B], Either[A, C]] = choiceZipping.right(pab)

    override def first[A, B, C](fa: Zipping[A, B]): Zipping[(A, C), (B, C)] = strongZipping.first(fa)

    override def second[A, B, C](fa: Zipping[A, B]): Zipping[(C, A), (C, B)] = strongZipping.second(fa)

    override def dimap[A, B, C, D](fab: Zipping[A, B])(f: C => A)(g: B => D): Zipping[C, D] =
      profunctorZipping.dimap(fab)(f)(g)
  }

  implicit final val closedZipping: Closed[Zipping] = new Closed[Zipping] {
    override def closed[A, B, C](pab: Zipping[A, B]): Zipping[C => A, C => B] =
      Zipping[C => A, C => B](f1 => f2 => x => pab.runZipping(f1(x))(f2(x)))

    override def dimap[A, B, C, D](fab: Zipping[A, B])(f: C => A)(g: B => D): Zipping[C, D] =
      profunctorZipping.dimap(fab)(f)(g)
  }
}

object Zipping extends ZippingInstances
