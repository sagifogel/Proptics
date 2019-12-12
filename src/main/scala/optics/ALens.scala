package optics

import optics.internal.Shop

/**
 * A Lens with fixed type [[Shop]] [[cats.arrow.Profunctor]]
 *
 * @tparam S the source of a [[ALens]]
 * @tparam T the modified source of a [[ALens]]
 * @tparam A the target of a [[ALens]]
 * @tparam B the modified target of a [[ALens]]
 */
abstract class ALens[S, T, A, B] extends Optic[Shop[A, B, *, *], S, T, A, B] { self =>
}
