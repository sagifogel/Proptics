package optics

import optics.internal.Indexed

/** An [[Indexed]] [[Optic]]
 *
 * @tparam P a type constructor of kind (* -> * -> *)
 * @tparam I the index of an [[IndexedOptic]]
 * @tparam S the source of an [[IndexedOptic]]
 * @tparam T the modified source of an [[IndexedOptic]]
 * @tparam A the target of an [[IndexedOptic]]
 * @tparam B the modified target of an [[IndexedOptic]]
 */
abstract class IndexedOptic[P[_, _], I, S, T, A, B] extends (Indexed[P, I, A, B] => P[S, T]) {
}
