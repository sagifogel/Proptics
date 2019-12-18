package optics

import optics.internal.Bazaar

/**
 * A [[Traversal]] with fixed type [[Bazaar]] [[cats.arrow.Profunctor]]
 *
 * @tparam S the source of a [[ATraversal]]
 * @tparam T the modified source of a [[ATraversal]]
 * @tparam A the target of a [[ATraversal]]
 * @tparam B the modified target of a [[ATraversal]]
 */
abstract class ATraversal[S, T, A, B] extends Optic[Bazaar[* => *, A, B, *, *], S, T, A, B]
