package proptics.internal

import cats.arrow.Profunctor
import proptics.profunctor.Closed

final case class Zipping[A, B](runZipping: A => A => B)

abstract class ZippingInstances {
  implicit final val profunctorZipping: Profunctor[Zipping] = new Profunctor[Zipping] {
    override def dimap[A, B, C, D](fab: Zipping[A, B])(f: C => A)(g: B => D): Zipping[C, D] =
      Zipping[C, D](c1 => c2 => g(fab.runZipping(f(c1))(f(c2))))
  }

  implicit final val closedZipping: Closed[Zipping] = new Closed[Zipping] {
    override def closed[A, B, C](pab: Zipping[A, B]): Zipping[C => A, C => B] =
      Zipping[C => A, C => B](f1 => f2 => x => pab.runZipping(f1(x))(f2(x)))

    override def dimap[A, B, C, D](fab: Zipping[A, B])(f: C => A)(g: B => D): Zipping[C, D] =
      profunctorZipping.dimap(fab)(f)(g)
  }
}

object Zipping extends ZippingInstances
