package proptics.internal

import cats.syntax.either._
import cats.{FlatMap, Functor}
import cats.arrow.Profunctor
import cats.data.Kleisli
import proptics.profunctor.Choice

/** The Market profunctor characterizes a [[proptics.Prism_]] */
final case class Market[A, B, S, T](review: B => T, viewOrModify: S => Either[T, A]) { self =>
  def compose[C, D](other: Market[C, D, A, B])(implicit ev: FlatMap[Either[T, *]]): Market[C, D, S, T] = {
    val kleisli = Kleisli[Either[T, *], A, C](other.viewOrModify(_).leftMap(self.review))

    Market(self.review compose other.review, (kleisli compose self.viewOrModify).run)
  }
}

abstract class MarketInstances {
  implicit final def functorMarket[C, D, S]: Functor[Market[C, D, S, *]] = new Functor[Market[C, D, S, *]] {
    override def map[A, B](fa: Market[C, D, S, A])(f: A => B): Market[C, D, S, B] =
      Market(f compose fa.review, fa.viewOrModify(_).leftMap(f))
  }

  implicit final def profunctorMarket[E, F]: Profunctor[Market[E, F, *, *]] = new Profunctor[Market[E, F, *, *]] {
    override def dimap[A, B, C, D](fab: Market[E, F, A, B])(f: C => A)(g: B => D): Market[E, F, C, D] =
      Market(g compose fab.review, c => fab.viewOrModify(f(c)).leftMap(g))
  }

  implicit final def choiceMarket[E, F]: Choice[Market[E, F, *, *]] = new Choice[Market[E, F, *, *]] {
    override def left[A, B, C](pab: Market[E, F, A, B]): Market[E, F, Either[A, C], Either[B, C]] =
      Market(pab.review(_).asLeft[C], _.fold(pab.viewOrModify(_).leftMap(_.asLeft[C]), _.asRight[B].asLeft[E]))

    override def right[A, B, C](pab: Market[E, F, A, B]): Market[E, F, Either[C, A], Either[C, B]] =
      Market[E, F, Either[C, A], Either[C, B]](
        pab.review(_).asRight[C],
        _.fold(_.asLeft[B].asLeft[E], pab.viewOrModify(_).fold(_.asRight[C].asLeft[E], _.asRight[Either[C, B]])))

    override def dimap[A, B, C, D](fab: Market[E, F, A, B])(f: C => A)(g: B => D): Market[E, F, C, D] =
      profunctorMarket.dimap(fab)(f)(g)
  }
}

object Market extends MarketInstances
