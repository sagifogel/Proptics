package optics

import optics.internal.Forget

/**
 * A [[IndexedFold]] is an [[IndexedOptic]] with fixed type [[Forget]] [[cats.arrow.Profunctor]]
 *
 * @tparam R the return type of an [[IndexedFold]]
 * @tparam I the index of an [[IndexedFold]]
 * @tparam S the source of an [[IndexedFold]]
 * @tparam T the modified source of an [[IndexedFold]]
 * @tparam A the target of an [[IndexedFold]]
 * @tparam B the modified target of an [[IndexedFold]]
 */
abstract class IndexedFold[R, I, S, T, A, B] extends IndexedOptic[Forget[R, *, *], I, S, T, A, B] {
}
