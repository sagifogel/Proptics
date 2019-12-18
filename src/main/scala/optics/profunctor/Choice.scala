package optics.profunctor

import cats.arrow.Profunctor

/**
 * The [[Choice]] class extends [[Profunctor]] with combinators for working with sum types.
 * @see [[Choice#left]] and [[Choice#right]] lift values in a [[Profunctor]] to act on the [[Left]] and
 * [[Right]] components of a sum, respectively.
 *
 * @tparam P a type constructor of kind (* -> * -> *)
 */
trait Choice[P[_, _]] extends Profunctor[P] {
  def left[A, B, C](pab: P[A, B]): P[Either[A, C], Either[B, C]]

  def right[A, B, C](pab: P[B, C]): P[Either[A, B], Either[A, C]]
}
