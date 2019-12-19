package optics

import optics.internal.Tagged

/**  A [[Review]] is an [[Optic]] with fixed type [[Tagged]] [[cats.arrow.Profunctor]]
 * 
 * @tparam S the source of an [[Review]]
 * @tparam T the modified source of an [[Review]]
 * @tparam A the target of an [[Review]]
 * @tparam B the modified target of an [[Review]]
 */
abstract class Review[S, T, A, B] extends Optic[Tagged[*, *], S, T, A, B] {
}
