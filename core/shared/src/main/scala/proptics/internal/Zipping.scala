package proptics.internal

import cats.Functor
import cats.arrow.{Profunctor, Strong}
import proptics.profunctor.Closed

/** The Zipping profunctor characterizes an [[proptics.Grate_]] */
final case class Zipping[A, B](runZipping: A => A => B) extends AnyVal

abstract class ZippingInstances {
  implicit final def functorZipping[C]: Functor[Zipping[C, *]] = new Functor[Zipping[C, *]] {
    override def map[A, B](fa: Zipping[C, A])(f: A => B): Zipping[C, B] =
      Zipping(f compose fa.runZipping(_))
  }

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

  implicit final val closedZipping: Closed[Zipping] = new Closed[Zipping] {
    override def closed[A, B, C](pab: Zipping[A, B]): Zipping[C => A, C => B] =
      Zipping[C => A, C => B](f1 => f2 => x => pab.runZipping(f1(x))(f2(x)))

    override def dimap[A, B, C, D](fab: Zipping[A, B])(f: C => A)(g: B => D): Zipping[C, D] =
      profunctorZipping.dimap(fab)(f)(g)
  }
}

object Zipping extends ZippingInstances
