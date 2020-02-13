package proptics

import cats.arrow.Strong
import cats.instances.function._
import cats.syntax.apply._
import proptics.internal.Indexed
import proptics.rank2types.Rank2TypeIndexedLensLike

/** [[IndexedLens]] is An IndexedOptic constrained with [[Strong]] [[cats.arrow.Profunctor]]
 *
 * @tparam P an evidence of [[Strong]] [[cats.arrow.Profunctor]]
 * @tparam I the index of an [[IndexedLens]]
 * @tparam S the source of an [[IndexedLens]]
 * @tparam T the modified source of an [[IndexedLens]]
 * @tparam A the target of an [[IndexedLens]]
 * @tparam B the modified target of an [[IndexedLens]]
 */
abstract class IndexedLens[ I, S, T, A, B] { self =>
  def apply[P[_, _]](index: Indexed[P, I, A, B])(implicit ev: Strong[P]): P[S, T]
}

object IndexedLens {
  private[proptics] def apply[I, S, T, A, B](f: Rank2TypeIndexedLensLike[I, S, T, A, B]): IndexedLens[I, S, T, A, B] = new IndexedLens[I, S, T, A, B] {
      override def apply[P[_, _]](index: Indexed[P, I, A, B])(implicit ev: Strong[P]): P[S, T] = f(index.runIndex)
    }

  def apply[I, S, T, A, B](to: S => ((I, A), B => T)): IndexedLens[I, S, T, A, B] = ilens_(to)

  def apply[I, S, T, A, B](get: S => (I, A))(set: S => B => T): IndexedLens[I, S, T, A, B] = ilens(get)(set)

  private[proptics] def ilens[I, S, T, A, B](get: S => (I, A))(set: S => B => T): IndexedLens[I, S, T, A, B] =
    ilens_((get, set).mapN(Tuple2.apply))

  private[proptics] def ilens_[I, S, T, A, B](to: S => ((I, A), B => T)): IndexedLens[I, S, T, A, B] =
    IndexedLens(new Rank2TypeIndexedLensLike[I, S, T, A, B] {
      override def apply[P[_, _]](piab: P[(I, A), B])(implicit ev: Strong[P]): P[S, T] =
        ev.dimap(ev.first[(I, A), B, B => T](piab))(to) { case (b, b2t) => b2t(b) }
    })
}
