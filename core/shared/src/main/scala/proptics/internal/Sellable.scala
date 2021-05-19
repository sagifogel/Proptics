package proptics.internal

import cats.Applicative
import cats.arrow.Profunctor

import proptics.profunctor.Corepresentable.Aux
import proptics.profunctor.Sellable

trait SellableInstances {
  implicit final def sellableBazaar[P[_, _], G[_]](implicit ev0: Profunctor[P], ev1: Aux[P, G]): Sellable[P, Bazaar[P, *, *, Unit, *]] = new Sellable[P, Bazaar[P, *, *, Unit, *]] {
    override def sell[A, B]: P[A, Bazaar[P, A, B, Unit, B]] =
      ev1.cotabulate { (ga: G[A]) =>
        new Bazaar[P, A, B, Unit, B] {
          override def runBazaar: RunBazaar[P, A, B, Unit, B] = new RunBazaar[P, A, B, Unit, B] {
            override def apply[F[_]](pafb: P[A, F[B]])(s: Unit)(implicit ev: Applicative[F]): F[B] =
              ev1.cosieve(pafb)(ga)
          }
        }
      }
  }
}

object Sellable extends SellableInstances {
  def apply[P[_, _], F[_]](implicit ev: Sellable[P, Bazaar[P, *, *, Unit, *]]): Sellable[P, Bazaar[P, *, *, Unit, *]] = ev
}
