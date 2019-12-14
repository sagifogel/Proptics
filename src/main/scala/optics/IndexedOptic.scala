package optics

import optics.internal.Indexed

/** An indexed [[Optic]]
 *
 * @tparam P a type constructor of kind (* -> * -> *)
 * @tparam I type index of an [[IndexedOptic]]
 * @tparam S the source of an [[IndexedOptic]]
 * @tparam T the modified source of a [[IndexedOptic]]
 * @tparam A the target of a [[IndexedOptic]]
 * @tparam B the modified target of a [[IndexedOptic]]
 */
abstract class IndexedOptic[P[_, _], I, S, T, A, B] extends (Indexed[P, I, A, B] => P[S, T]) {
}
