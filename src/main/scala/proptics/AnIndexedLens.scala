package proptics

import cats.arrow.Strong
import proptics.internal.{Indexed, Shop}

import scala.Function.const

/**
 * An [[IndexedOptic]] with fixed type [[Shop]] [[cats.arrow.Profunctor]]
 *
 * @tparam I the index of an [[AnIndexedLens]]
 * @tparam S the source of an [[AnIndexedLens]]
 * @tparam T the modified source of an [[AnIndexedLens]]
 * @tparam A the target of an [[AnIndexedLens]]
 * @tparam B the modified target of an [[AnIndexedLens]]
 */
abstract class AnIndexedLens[I, S, T, A, B] extends IndexedOptic[Shop[(I, A), B, *, *], I, S, T, A, B] { self =>
  def withIndexedLens[R](f: (S => (I, A)) => (S => B => T) => R): R = {
    val shop = self(Indexed(Shop(identity, const(identity))))

    f(shop.get)(shop.set)
  }

  def clone[P[_, _]](implicit ev: Strong[P]): IndexedLens[I, S, T, A, B] = {
    withIndexedLens(IndexedLens.ilens[I, S, T, A, B])
  }
}
