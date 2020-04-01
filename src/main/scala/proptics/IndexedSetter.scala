package proptics

import proptics.internal.Indexed

import scala.Function.const

/**
 * An [[IndexedSetter]] is an indexed optic with a fixed type of a [[Function1]] as the type constructor
 *
 * @tparam S the source of an [[IndexedSetter]]
 * @tparam I the index of an [[IndexedSetter]]
 * @tparam T the modified source of an [[IndexedSetter]]
 * @tparam A the target of an [[IndexedSetter]]
 * @tparam B the modified target of an [[IndexedSetter]]
 */
abstract class IndexedSetter[I, S, T, A, B] extends Serializable { self =>
  private[proptics] def apply(indexed: Indexed[* => *, I, A, B]): S => T

  def set(b: B): S => T = over(const(b))

  def over(f: ((I, A)) => B): S => T = self(Indexed(f))
}

object IndexedSetter {
  private[proptics] def apply[I, S, T, A, B](f: Indexed[* => *, I, A, B] => S => T): IndexedSetter[I, S, T, A, B] = new IndexedSetter[I, S, T, A, B] {
    override def apply(indexed: Indexed[* => *, I, A, B]): S => T = f(indexed)
  }

  def apply[I, S, T, A, B](get: ((I, A) => B) => S => T)(implicit ev: DummyImplicit): IndexedSetter[I, S, T, A, B] =
    IndexedSetter((indexed: Indexed[* => *, I, A, B]) => {
      get { case (i, a) => indexed.runIndex(i, a) }
    })
}

object IndexedSetter_ {
  def apply[I, S, A](get: ((I, A) => A) => S => S): IndexedSetter_[I, S, A] =
    IndexedSetter((indexed: Indexed[* => *, I, A, A]) => get { case (i, a) => indexed.runIndex(i, a) })
}