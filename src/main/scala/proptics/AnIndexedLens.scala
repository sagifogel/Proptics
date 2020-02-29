package proptics

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
abstract class AnIndexedLens[I, S, T, A, B] { self =>
  def apply(indexed: Indexed[Shop[(I, A), B, *, *], I, A, B]): Shop[(I, A), B, S, T]

  def withIndexedLens[R](f: (S => (I, A)) => (S => B => T) => R): R = {
    val shop = self(Indexed(Shop(identity, const(identity))))

    f(shop.get)(shop.set)
  }

  def cloneIndexedLens: IndexedLens[I, S, T, A, B] =
    withIndexedLens(IndexedLens.apply[I, S, T, A, B])
}

object AnIndexedLens {
  def apply[I, S, T, A, B](get: S => (I, A))(set: S => B => T): AnIndexedLens[I, S, T, A, B] = new AnIndexedLens[I, S, T, A, B] {
    override def apply(indexed: Indexed[Shop[(I, A), B, *, *], I, A, B]): Shop[(I, A), B, S, T] = {
      val idx = indexed.runIndex

      Shop(idx.get compose get, s => b => {
        val b2 = idx.set(get(s))(b)

        set(s)(b2)
      })
    }
  }
}

object AnIndexedLens_ {
  def apply[I, S, A](get: S => (I, A))(set: S => A => S): AnIndexedLens_[I, S, A] = AnIndexedLens(get)(set)
}
