package optics

import cats.arrow.Strong
import cats.instances.function._
import cats.syntax.apply._
import optics.internal.Indexed

/** [[IndexedLens]] is An IndexedOptic constrained with [[Strong]] [[cats.arrow.Profunctor]]
 *
 * @tparam P an evidence of [[Strong]] [[cats.arrow.Profunctor]]
 * @tparam I the index of an [[IndexedLens]]
 * @tparam S the source of an [[IndexedLens]]
 * @tparam T the modified source of an [[IndexedLens]]
 * @tparam A the target of an [[IndexedLens]]
 * @tparam B the modified target of an [[IndexedLens]]
 */
abstract class IndexedLens[P[_, _] : Strong, I, S, T, A, B] extends IndexedOptic[P, I, S, T, A, B] { self =>
}

object IndexedLens {
  private[optics] def apply[P[_, _], I, S, T, A, B](f: P[(I, A), B] => P[S, T])(implicit ev: Strong[P]): IndexedLens[P, I, S, T, A, B] =
    new IndexedLens[P, I, S, T, A, B] {
      override def apply(index: Indexed[P, I, A, B]): P[S, T] = f(index.runIndex)
    }

  def apply[P[_, _], I, S, T, A, B](to: S => ((I, A), B => T))(implicit ev: Strong[P], d: DummyImplicit): IndexedLens[P, I, S, T, A, B] =
    ilens_(to)

  def apply[P[_, _], I, S, T, A, B](get: S => (I, A))(set: S => B => T)(implicit ev: Strong[P]): IndexedLens[P, I, S, T, A, B] =
    ilens(get)(set)

  private[optics] def ilens[P[_, _], I, S, T, A, B](get: S => (I, A))(set: S => B => T)(implicit ev: Strong[P]): IndexedLens[P, I, S, T, A, B] =
    ilens_((get, set).mapN(Tuple2.apply))

  private[optics] def ilens_[P[_, _], I, S, T, A, B](to: S => ((I, A), B => T))(implicit ev: Strong[P]): IndexedLens[P, I, S, T, A, B] =
    IndexedLens[P, I, S, T, A, B]((piab: P[(I, A), B]) => {
      ev.dimap(ev.first[(I, A), B, B => T](piab))(to) { case (b, b2t) => b2t(b) }
    })
}
