package optics.internal

import cats.arrow.{Profunctor, Strong}

/** The [[Shop]] profunctor characterizes a `Lens`  */
final case class Shop[A, B, S, T](f: S => A, g: S => B => T)

abstract class ShopInstances {
  implicit final def profunctorShop[E, F]: Profunctor[Shop[E, F, *, *]] = new Profunctor[Shop[E, F, *, *]] {
    override def dimap[A, B, C, D](fab: Shop[E, F, A, B])(f: C => A)(g: B => D): Shop[E, F, C, D] =
      Shop(fab.f compose f, s => g compose fab.g(f(s)))
  }

  implicit final def strongShop[E, F]: Strong[Shop[E, F, *, *]] = new Strong[Shop[E, F, *, *]] {
    override def first[A, B, C](fa: Shop[E, F, A, B]): Shop[E, F, (A, C), (B, C)] =
      Shop({ case (a, _) => fa.f(a) }, { case (a, c) => e => (fa.g(a)(e), c) })

    override def second[A, B, C](fa: Shop[E, F, A, B]): Shop[E, F, (C, A), (C, B)] =
      Shop({ case (_, a) => fa.f(a) }, { case (c, a) => e => (c, fa.g(a)(e)) })

    override def dimap[A, B, C, D](fab: Shop[E, F, A, B])(f: C => A)(g: B => D): Shop[E, F, C, D] =
      profunctorShop.dimap(fab)(f)(g)
  }
}

object Shop extends ShopInstances
