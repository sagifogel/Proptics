package optics

import optics.internal.Grating

/**
 * An [[Optic] with fixed type [[Grating]] [[cats.arrow.Profunctor]]
 *
 * @tparam S the source of an [[AGrate]]
 * @tparam T the modified source of an [[AGrate]]
 * @tparam A the target of an [[AGrate]]
 * @tparam B the modified target of an [[AGrate]]
 */
abstract class AGrate[S, T, A, B] extends Optic[Grating[A, B, *, *], S, T, A, B] {
}
