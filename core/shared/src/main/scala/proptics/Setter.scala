package proptics

import cats.instances.function._
import cats.syntax.either._
import cats.{Contravariant, Functor}
import proptics.internal.{Exchange, Market, Shop}

import scala.Function.const

/**
  * A [[Setter_]] A Setter is a generalization of fmap from [[Functor]]
  *
  * @tparam S the source of a [[Setter_]]
  * @tparam T the modified source of a [[Setter_]]
  * @tparam A the target of a [[Setter_]]
  * @tparam B the modified target of a [[Setter_]]
  */
abstract class Setter_[S, T, A, B] extends Serializable { self =>
  private[proptics] def apply(pab: A => B): S => T

  def set(b: B): S => T = over(const(b))

  def over(f: A => B): S => T = self(f)

  def compose[C, D](other: Setter_[A, B, C, D]): Setter_[S, T, C, D] = new Setter_[S, T, C, D] {
    override private[proptics] def apply(pab: C => D) = self(other(pab))
  }

  def compose[C, D](other: Iso_[A, B, C, D]): Setter_[S, T, C, D] = new Setter_[S, T, C, D] {
    override private[proptics] def apply(pab: C => D) = self(other(pab))
  }

  def compose[C, D](other: AnIso_[A, B, C, D]): Setter_[S, T, C, D] = new Setter_[S, T, C, D] {
    override private[proptics] def apply(pab: C => D): S => T = {
      val exchange = other(Exchange(identity, identity))

      self.over(exchange.inverseGet compose pab compose exchange.get)
    }
  }

  def compose[C, D](other: Lens_[A, B, C, D]): Setter_[S, T, C, D] = new Setter_[S, T, C, D] {
    override private[proptics] def apply(pab: C => D): S => T = self(other(pab))
  }

  def compose[C, D](other: ALens_[A, B, C, D]): Setter_[S, T, C, D] = new Setter_[S, T, C, D] {
    override private[proptics] def apply(pab: C => D): S => T = {
      val shop = other(Shop(identity, const(identity)))

      self.over(a => shop.set(a)(pab(shop.get(a))))
    }
  }

  def compose[C, D](other: Prism_[A, B, C, D]): Setter_[S, T, C, D] = new Setter_[S, T, C, D] {
    override private[proptics] def apply(pab: C => D): S => T = self(other(pab))
  }

  def compose[C, D](other: APrism_[A, B, C, D]): Setter_[S, T, C, D] = new Setter_[S, T, C, D] {
    override private[proptics] def apply(pab: C => D): S => T = {
      val market = other(Market(identity, _.asRight[D]))

      self.over(market.from(_).fold(identity, market.to compose pab))
    }
  }

  def compose[C, D](other: Traversal_[A, B, C, D]): Setter_[S, T, C, D] = new Setter_[S, T, C, D] {
    override private[proptics] def apply(pab: C => D): S => T = self(other(pab))
  }

  def compose[C, D](other: ATraversal_[A, B, C, D]): Setter_[S, T, C, D] = self compose other.asTraversal_

  def compose[C, D](other: Grate_[A, B, C, D]): Setter_[S, T, C, D] = new Setter_[S, T, C, D] {
    override private[proptics] def apply(pab: C => D): S => T = self(other(pab))
  }

  def compose[C, D](other: AGrate_[A, B, C, D]): Setter_[S, T, C, D] = self compose other.asGrate_
}

object Setter_ {
  def apply[S, T, A, B](f: (A => B) => S => T): Setter_[S, T, A, B] = new Setter_[S, T, A, B] {
    override def apply(pab: A => B): S => T = f(pab)
  }

  def fromFunctor[F[_], A, B](implicit ev: Functor[F]): Setter_[F[A], F[B], A, B] = Setter_(ev.lift)

  def fromContravariant[F[_], A, B](implicit ev: Contravariant[F]): Setter_[F[B], F[A], A, B] =
    Setter_(ev.liftContravariant)
}

object Setter {
  def apply[S, A](f: (A => A) => S => S): Setter_[S, S, A, A] = Setter_[S, S, A, A](f)
}
