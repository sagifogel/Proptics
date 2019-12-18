package optics

import optics.internal.Forget

/**
 * A [[Fold]] is an [[Optic]] with fixed type [[Forget]] [[cats.arrow.Profunctor]]
 *
 * @tparam R the return type of a [[Fold]]
 * @tparam S the source of a [[Fold]]
 * @tparam T the modified source of a [[Fold]]
 * @tparam A the target of a [[Fold]]
 * @tparam B the modified target of a [[Fold]]
 */
abstract class Fold[R, S, T, A, B] extends Optic[Forget[R, *, *], S, T, A, B] {
}
