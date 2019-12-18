package optics

import optics.internal.Shop

/**
 * An [[IndexedOptic]] with fixed type [[Shop]] [[cats.arrow.Profunctor]]
 *
 * @tparam S the source of an [[AnIndexedLens]]
 * @tparam T the modified source of an [[AnIndexedLens]]
 * @tparam A the target of an [[AnIndexedLens]]
 * @tparam B the modified target of an [[AnIndexedLens]]
 */
abstract class AnIndexedLens[I, S, T, A, B] extends IndexedOptic[Shop[(I, A), B, *, *], I, S, T, A, B] {
}
