package optics

import cats.arrow.Profunctor

/**
 * A generalized isomorphism
 *
 * @tparam P an evidence of [[Profunctor]]
 * @tparam S the source of an [[Iso]]
 * @tparam T the modified source of an [[Iso]]
 * @tparam A the target of a [[Iso]]
 * @tparam B the modified target of a [[Iso]]
 */
abstract class Iso[P[_, _]: Profunctor , S, T, A, B] extends Optic[P, S, T, A, B] { self =>
}
