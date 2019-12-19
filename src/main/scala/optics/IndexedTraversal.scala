package optics

import optics.internal.Wander

/**
 * An [[IndexedTraversal]] is An IndexedOptic constrained with [[Wander]] [[cats.arrow.Profunctor]]
 *
 * @tparam P an evidence of [[Wander]]
 * @tparam I the index of an [[IndexedTraversal]]
 * @tparam S the source of an [[IndexedTraversal]]
 * @tparam T the modified source of an [[IndexedTraversal]]
 * @tparam A the target of an [[IndexedTraversal]]
 * @tparam B the modified target of an [[IndexedTraversal]]
 */
abstract class IndexedTraversal[P[_, _]: Wander, I, S, T, A, B] extends IndexedOptic[P, I, S, T, A, B] {
}
