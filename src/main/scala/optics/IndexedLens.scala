package optics

import cats.arrow.Strong

/** [[IndexedLens]] is An IndexedOptic constrained with [[Strong]] [[cats.arrow.Profunctor]]
 *
 * @tparam P an evidence of [[Strong]] [[cats.arrow.Profunctor]]
 * @tparam I the index of an [[IndexedLens]]
 * @tparam S the source of an [[IndexedLens]]
 * @tparam T the modified source of an [[IndexedLens]]
 * @tparam A the target of an [[IndexedLens]]
 * @tparam B the modified target of an [[IndexedLens]]
 */
abstract class IndexedLens[P[_, _]: Strong, I , S, T, A, B] extends IndexedOptic[P, I, S, T, A, B] {
}
