package optics

import optics.internal.Wander

/**
 *
 * @tparam P an evidence of [[Wander]]
 * @tparam S the source of a [[Traversal]]
 * @tparam T the modified source of a [[Traversal]]
 * @tparam A the target of a [[Traversal]]
 * @tparam B the modified target of a [[Traversal]]
 */
abstract class Traversal[P[_, _]: Wander, S, T, A, B] extends Optic[P, S, T, A, B] {
}
