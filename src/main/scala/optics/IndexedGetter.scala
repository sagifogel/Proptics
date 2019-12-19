package optics

/**
 * An [[IndexedGetter]] is an [[IndexedFold]]
 *
 * @tparam I the index of an [[IndexedGetter]]
 * @tparam S the source of an [[IndexedGetter]]
 * @tparam T the modified source of an [[IndexedGetter]]
 * @tparam A the target of an [[IndexedGetter]]
 * @tparam B the modified target of an [[IndexedGetter]]
 */
abstract class IndexedGetter[I, S, T, A, B] extends IndexedFold[A, I, S, T, A, B] {
}
