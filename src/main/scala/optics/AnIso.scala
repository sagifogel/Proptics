package optics

import optics.internal.Exchange

/**
 * An Iso with fixed type [[Exchange]] [[cats.arrow.Profunctor]]
 *
 * @tparam S the source of a [[AnIso]]
 * @tparam T the modified source of a [[AnIso]]
 * @tparam A the target of a [[AnIso]]
 * @tparam B the modified target of a [[AnIso]]
 */
abstract class AnIso[S, T, A, B] extends Optic[Exchange[A, B, *, *], S, T, A, B] { self =>
}
