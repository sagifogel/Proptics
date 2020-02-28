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
abstract class IndexedLens[I, S, T, A, B] { self =>
  def apply[P[_, _]](index: Indexed[P, I, A, B])(implicit ev: Strong[P]): P[S, T]
}

object IndexedLens {
  private[proptics] def apply[I, S, T, A, B](f: Rank2TypeIndexedLensLike[I, S, T, A, B]): IndexedLens[I, S, T, A, B] = new IndexedLens[I, S, T, A, B] {
    override def apply[P[_, _]](index: Indexed[P, I, A, B])(implicit ev: Strong[P]): P[S, T] = f(index.runIndex)
  }

  def apply[I, S, T, A, B](to: S => ((I, A), B => T)): IndexedLens[I, S, T, A, B] =
    IndexedLens(new Rank2TypeIndexedLensLike[I, S, T, A, B] {
      override def apply[P[_, _]](piab: P[(I, A), B])(implicit ev: Strong[P]): P[S, T] =
        liftIndexedOptic(to)(ev)(piab)
    })

  def apply[I, S, T, A, B](get: S => (I, A))(set: S => B => T): IndexedLens[I, S, T, A, B] =
    IndexedLens((get, set).mapN(Tuple2.apply))

  private[proptics] def liftIndexedOptic[P[_, _], I, S, T, A, B](to: S => ((I, A), B => T))(implicit ev: Strong[P]): P[(I, A), B] => P[S, T] =
    piab => {
      ev.dimap(ev.first[(I, A), B, B => T](piab))(to) { case (b, b2t) => b2t(b) }
    }
}

object IndexedLens_ {
  def apply[I, S, A](to: S => ((I, A), A => S)): IndexedLens_[I, S, A] = IndexedLens(to)

  def apply[I, S, A](get: S => (I, A))(set: S => A => S): IndexedLens_[I, S, A] = IndexedLens(get)(set)
}
