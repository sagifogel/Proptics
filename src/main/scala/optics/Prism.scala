package optics

import cats.arrow.Choice

/**
 * @tparam P an evidence of [[Choice]]
 * @tparam S the source of a [[Prism]]
 * @tparam T the modified source of a [[Prism]]
 * @tparam A the target of a [[Prism]]
 * @tparam B the modified target of a [[Prism]]
 */
abstract class Prism[P[_, _]: Choice, S, T, A, B] extends Optic[P, S, T, A, B] { self =>
}


