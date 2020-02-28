package proptics

import cats.arrow.Profunctor
import proptics.internal.Indexed

/** An [[Indexed]] [[Optic]]
 *
 * @tparam P a type constructor of kind (* -> * -> *)
 * @tparam I the index of an [[IndexedOptic]]
 * @tparam S the source of an [[IndexedOptic]]
 * @tparam T the modified source of an [[IndexedOptic]]
 * @tparam A the target of an [[IndexedOptic]]
 * @tparam B the modified target of an [[IndexedOptic]]
 */
private[proptics] abstract class IndexedOptic[P[_, _], I, S, T, A, B] { self =>
  def apply(index: Indexed[P, I, A, B]): P[S, T]

  def unIndex(implicit ev: Profunctor[P]): Optic[P, S, T, A, B] =
    Optic(pab => self(Indexed(ev.dimap[A, B, (I, A), B](pab)(_._2)(identity))))
}

object IndexedOptic {
  def apply[P[_, _], I, S, T, A, B](f: Indexed[P, I, A, B] => P[S, T]): IndexedOptic[P, I, S, T, A, B] = new IndexedOptic[P, I, S, T, A, B] {
    override def apply(index: Indexed[P, I, A, B]): P[S, T] = f(index)
  }
}

object IndexedOptic_ {
  def apply[P[_, _], I, S, A](f: Indexed[P, I, A, A] => P[S, S]): IndexedOptic_[P, I, S, A] = IndexedOptic(f)
}
