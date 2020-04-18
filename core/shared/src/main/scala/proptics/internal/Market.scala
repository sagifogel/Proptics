package proptics.internal

import cats.syntax.either._
import cats.{FlatMap, Functor}
import cats.arrow.Profunctor
import cats.data.Kleisli
import proptics.profunctor.Choice

/** The [[Market]] profunctor characterizes an [[proptics.Prism]] */
final case class Market[A, B, S, T](to: B => T, from: S => Either[T, A]) { self =>
  def compose[C, D](other: Market[C, D, A, B])(implicit ev: FlatMap[Either[T, *]]): Market[C, D, S, T] = {
    val kleisli = Kleisli[Either[T, *], A, C](other.from(_).leftMap(self.to))

    Market(self.to compose other.to, (kleisli compose self.from).run)
  }
}

abstract class MarketInstances {
  implicit final def functorMarket[C, D, S]: Functor[Market[C, D, S, *]] = new Functor[Market[C, D, S, *]] {
    override def map[A, B](fa: Market[C, D, S, A])(f: A => B): Market[C, D, S, B] =
      Market(f compose fa.to, fa.from(_).leftMap(f))
  }

  implicit final def profunctorMarket[E, F]: Profunctor[Market[E, F, *, *]] = new Profunctor[Market[E, F, *, *]] {
    override def dimap[A, B, C, D](fab: Market[E, F, A, B])(f: C => A)(g: B => D): Market[E, F, C, D] = {
      Market(g compose fab.to, c => fab.from(f(c)).leftMap(g))
    }
  }

  implicit final def choiceMarket[E, F]: Choice[Market[E, F, *, *]] = new Choice[Market[E, F, *, *]] {
    override def left[A, B, C](pab: Market[E, F, A, B]): Market[E, F, Either[A, C], Either[B, C]] =
      Market(pab.to(_).asLeft[C], _.fold(pab.from(_).leftMap(_.asLeft[C]), _.asRight[B].asLeft[E]))

    override def right[A, B, C](pab: Market[E, F, B, C]): Market[E, F, Either[A, B], Either[A, C]] =
      Market(pab.to(_).asRight[A], _.fold(_.asLeft[C].asLeft[E], pab.from(_).leftMap(_.asRight[A])))

    override def dimap[A, B, C, D](fab: Market[E, F, A, B])(f: C => A)(g: B => D): Market[E, F, C, D] =
      profunctorMarket.dimap(fab)(f)(g)
  }
}

object Market extends MarketInstances