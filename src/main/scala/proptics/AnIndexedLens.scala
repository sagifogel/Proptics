package proptics

import cats.syntax.eq._
import cats.syntax.option._
import cats.{Applicative, Eq, Id}
import proptics.internal.{Indexed, Shop}

import scala.Function.const

/**
 * An [[IndexedOptic]] with fixed type [[Shop]] [[cats.arrow.Profunctor]]
 *
 * @tparam I the index of an [[AnIndexedLens_]]
 * @tparam S the source of an [[AnIndexedLens_]]
 * @tparam T the modified source of an [[AnIndexedLens_]]
 * @tparam A the target of an [[AnIndexedLens_]]
 * @tparam B the modified target of an [[AnIndexedLens_]]
 */
abstract class AnIndexedLens_[I, S, T, A, B] { self =>
  def apply(indexed: Indexed[Shop[(I, A), B, *, *], I, A, B]): Shop[(I, A), B, S, T]

  def view(s: S): (I, A) = applyShop.get(s)

  def set(b: B): S => T = over(const(b))

  def over(f: ((I, A)) => B): S => T = overF[Id](f)

  def overF[F[_]: Applicative](f: ((I, A)) => F[B])(s: S): F[T] = traverse(s)(f)

  def traverse[F[_]](s: S)(f: ((I, A)) => F[B])(implicit ev: Applicative[F]): F[T] =
    ev.map(f(applyShop.get(s)))(set(_)(s))

  def filter(f: ((I, A)) => Boolean): S => Option[(I, A)] = s => view(s).some.filter(f)

  def exists(f: ((I, A)) => Boolean): S => Boolean = f compose view

  def noExists(f: ((I, A)) => Boolean): S => Boolean = s => !exists(f)(s)

  def contains(s: S)(a: (I, A))(implicit ev: Eq[(I, A)]): Boolean = exists(_ === a)(s)

  def notContains(s: S)(a: (I, A))(implicit ev: Eq[(I, A)]): Boolean = !contains(s)(a)

  def withIndexedLens[R](f: (S => (I, A)) => (S => B => T) => R): R = {
    val shop = applyShop

    f(shop.get)(shop.set)
  }

  def asIndexedLens_ : IndexedLens_[I, S, T, A, B] = withIndexedLens(IndexedLens_[I, S, T, A, B])

  def asLens_ : Lens_[S, T, A, B] = withIndexedLens(sia => sbt => Lens_(s => (sia(s)._2, sbt(s))))

  private def applyShop: Shop[(I, A), B, S, T] = self(Indexed(Shop(identity, const(identity))))
}

object AnIndexedLens_ {
  def apply[I, S, T, A, B](get: S => (I, A))(_set: S => B => T): AnIndexedLens_[I, S, T, A, B] = new AnIndexedLens_[I, S, T, A, B] {
    override def apply(indexed: Indexed[Shop[(I, A), B, *, *], I, A, B]): Shop[(I, A), B, S, T] = {
      val idx = indexed.runIndex

      Shop(idx.get compose get, s => b => {
        val b2 = idx.set(get(s))(b)

        _set(s)(b2)
      })
    }
  }
}

object AnIndexedLens {
  def apply[I, S, A](get: S => (I, A))(set: S => A => S): AnIndexedLens[I, S, A] = AnIndexedLens_(get)(set)
}
