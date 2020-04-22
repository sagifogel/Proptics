package proptics

import cats.instances.function._
import cats.syntax.apply._
import cats.syntax.eq._
import cats.syntax.option._
import cats.{Applicative, Eq, Functor, Id}
import proptics.internal.{Shop, Traversing, Wander}

import scala.Function.const

/**
  * A [[Lens_]] with fixed type [[Shop]] [[cats.arrow.Profunctor]]
  *
  * @tparam S the source of a [[ALens_]]
  * @tparam T the modified source of a [[ALens_]]
  * @tparam A the target of a [[ALens_]]
  * @tparam B the modified target of a [[ALens_]]
  */
abstract class ALens_[S, T, A, B] { self =>
  def apply(shop: Shop[A, B, A, B]): Shop[A, B, S, T]

  def view(s: S): A = self(Shop(identity, const(identity))).get(s)

  def set(b: B): S => T = over(const(b))

  def over(f: A => B): S => T = s => overF[Id](f)(s)

  def overF[F[_]: Functor](f: A => F[B])(s: S): F[T] = traverse(s)(f)

  def traverse[F[_]: Functor](s: S)(f: A => F[B])(implicit ev: Functor[F]): F[T] = {
    val shop = self(Shop(identity[A], const(identity[B])))

    ev.map(f(shop.get(s)))(shop.set(s))
  }

  def filter(f: A => Boolean): S => Option[A] = s => view(s).some.filter(f)

  def exists(f: A => Boolean): S => Boolean = f compose view

  def noExists(f: A => Boolean): S => Boolean = s => !exists(f)(s)

  def contains(s: S)(a: A)(implicit ev: Eq[A]): Boolean = exists(_ === a)(s)

  def notContains(s: S)(a: A)(implicit ev: Eq[A]): Boolean = !contains(s)(a)

  def withLens[R](f: (S => A) => (S => B => T) => R): R = {
    val shop = toShop

    f(shop.get)(shop.set)
  }

  def asLens_ : Lens_[S, T, A, B] = withLens(Lens_[S, T, A, B])

  /**
    * Converts a [[Lens_]] into the form that [[Lens]] accepts.
    *
    * Can be useful when defining a lens where the focus appears under multiple
    * constructors of an algebraic data type. This function would be called for
    * each case of the data type.
    */
  def lensStore(s: S): (A, B => T) = withLens(sa => sbt => (sa, sbt).mapN(Tuple2.apply))(s)

  def compose[C, D](other: Iso_[A, B, C, D]): ALens_[S, T, C, D] = new ALens_[S, T, C, D] {
    override def apply(shop: Shop[C, D, C, D]): Shop[C, D, S, T] =
      self(Shop(identity, const(identity))) compose other(shop)
  }

  def compose[C, D](other: AnIso_[A, B, C, D]): ALens_[S, T, C, D] = self compose other.asIso_

  def compose[C, D](other: Lens_[A, B, C, D]): ALens_[S, T, C, D] = new ALens_[S, T, C, D] {
    override def apply(shop: Shop[C, D, C, D]): Shop[C, D, S, T] =
      self(Shop(identity, const(identity))) compose other(shop)
  }

  def compose[C, D](other: ALens_[A, B, C, D]): ALens_[S, T, C, D] = new ALens_[S, T, C, D] {
    override def apply(shop: Shop[C, D, C, D]): Shop[C, D, S, T] =
      self(Shop(identity, const(identity))) compose other(shop)
  }

  def compose[C, D](other: Prism_[A, B, C, D]): Traversal_[S, T, C, D] = new Traversal_[S, T, C, D] {
    override def apply[P[_, _]](pab: P[C, D])(implicit ev: Wander[P]): P[S, T] = {
      val traversing = new Traversing[S, T, C, D] {
        override def apply[F[_]](f: C => F[D])(s: S)(implicit ev: Applicative[F]): F[T] =
          self.traverse(s)(other.traverse(_)(f))
      }

      ev.wander(traversing)(pab)
    }
  }

  def compose[C, D](other: APrism_[A, B, C, D]): Traversal_[S, T, C, D] = self compose other.asPrism_

  private[this] def toShop: Shop[A, B, S, T] = self(Shop(identity, const(identity)))
}

object ALens_ {
  private[proptics] def apply[S, T, A, B](f: Shop[A, B, A, B] => Shop[A, B, S, T]): ALens_[S, T, A, B] = new ALens_[S, T, A, B] { self =>
    override def apply(shop: Shop[A, B, A, B]): Shop[A, B, S, T] = f(shop)
  }

  def apply[S, T, A, B](get: S => A)(set: S => B => T): ALens_[S, T, A, B] =
    ALens_ { shop =>
      Shop(
        shop.get compose get,
        s =>
          b => {
            val a = get(s)

            set(s)(shop.set(a)(b))
          })
    }
}

object ALens {
  def apply[S, A](get: S => A)(set: S => A => S): ALens[S, A] = ALens_(get)(set)
}
