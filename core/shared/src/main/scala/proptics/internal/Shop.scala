package proptics.internal

import cats.arrow.{Profunctor, Strong}

/** The Shop profunctor characterizes a [[proptics.Lens_]] */
final case class Shop[A, B, S, T](view: S => A, set: S => B => T) {
  def andThen[C, D](other: Shop[C, D, A, B]): Shop[C, D, S, T] =
    Shop(other.view compose view, s => d => set(s)(other.set(view(s))(d)))
}

abstract class ShopInstances {
  implicit final def profunctorShop[E, F]: Profunctor[Shop[E, F, *, *]] = new Profunctor[Shop[E, F, *, *]] {
    override def dimap[A, B, C, D](fab: Shop[E, F, A, B])(f: C => A)(g: B => D): Shop[E, F, C, D] =
      Shop(fab.view compose f, c => g compose fab.set(f(c)))
  }

  implicit final def strongShop[E, F]: Strong[Shop[E, F, *, *]] = new Strong[Shop[E, F, *, *]] {
    override def first[A, B, C](fa: Shop[E, F, A, B]): Shop[E, F, (A, C), (B, C)] =
      Shop({ case (a, _) => fa.view(a) }, { case (a, c) => e => (fa.set(a)(e), c) })

    override def second[A, B, C](fa: Shop[E, F, A, B]): Shop[E, F, (C, A), (C, B)] =
      Shop({ case (_, a) => fa.view(a) }, { case (c, a) => e => (c, fa.set(a)(e)) })

    override def dimap[A, B, C, D](fab: Shop[E, F, A, B])(f: C => A)(g: B => D): Shop[E, F, C, D] =
      profunctorShop.dimap(fab)(f)(g)
  }
}

object Shop extends ShopInstances
