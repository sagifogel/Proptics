package proptics.internal

import scala.annotation.implicitNotFound

import cats.Applicative
import cats.arrow.Profunctor

import proptics.profunctor.Corepresentable.Aux

/** The [[Sellable]] is used internally to construct a [[Bazaar]] */
@implicitNotFound("Could not find an instance of Sellable[${P}, ${W}]")
trait Sellable[P[_, _], W[_, _, _]] extends Serializable {
  def sell[A, B]: P[A, W[A, B, B]]
}

abstract class SellableInstances {
  implicit final def sellableBazaar[P[_, _]: Profunctor, G[_]](implicit ev1: Aux[P, G]): Sellable[P, Bazaar[P, *, *, Unit, *]] = new Sellable[P, Bazaar[P, *, *, Unit, *]] {
    override def sell[A, B]: P[A, Bazaar[P, A, B, Unit, B]] =
      ev1.cotabulate { (ga: G[A]) =>
        new Bazaar[P, A, B, Unit, B] {
          override def runBazaar: RunBazaar[P, A, B, Unit, B] = new RunBazaar[P, A, B, Unit, B] {
            override def apply[F[_]](pafb: P[A, F[B]])(s: Unit)(implicit ev2: Applicative[F]): F[B] =
              ev1.cosieve(pafb)(ga)
          }
        }
      }
  }
}

object Sellable extends SellableInstances {
  /** summon an instance of [[Sellable]] for `P` */
  @inline def apply[P[_, _]](implicit ev0: Sellable[P, Bazaar[P, *, *, Unit, *]]): Sellable[P, Bazaar[P, *, *, Unit, *]] = ev0
}
