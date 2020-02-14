package proptics

import proptics.internal.Indexed

/**
 * An [[IndexedSetter]] is an [[IndexedOptic]] with a fixed type of a [[Function1]] as the type constructor
 *
 * @tparam S the source of an [[IndexedSetter]]
 * @tparam I the index of an [[IndexedSetter]]
 * @tparam T the modified source of an [[IndexedSetter]]
 * @tparam A the target of an [[IndexedSetter]]
 * @tparam B the modified target of an [[IndexedSetter]]
 */
abstract class IndexedSetter[I, S, T, A, B] { self =>
  def apply(indexed: Indexed[* => *, I, A, B]): S => T

  def over(f: (I, A) => B): S => T = self(Indexed(f.tupled))
}

object IndexedSetter {
  private[proptics] def apply[I, S, T, A, B](f: Indexed[* => *, I, A, B] => S => T): IndexedSetter[I, S, T, A, B] = new IndexedSetter[I, S, T, A, B] {
    override def apply(indexed: Indexed[* => *, I, A, B]): S => T = f(indexed)
  }

  def apply[I, S, T, A, B](get: ((I, A) => B) => S => T): IndexedSetter[I, S, T, A, B] =
    IndexedSetter((indexed: Indexed[* => *, I, A, B]) => {
      get { case (i, a) => indexed.runIndex(i, a) }
    })
}