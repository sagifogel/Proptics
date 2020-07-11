package proptics.profunctor

import cats.arrow.Profunctor

/**
  * The [[Cochoice]] type class provides the dual operations of the [[Choice]].
  *
  * @tparam P a type constructor of kind (* -> * -> *)
  * */
trait Cochoice[P[_, _]] extends Profunctor[P] {
  def unleft[A, B, C](p: P[Either[A, C], Either[B, C]]): P[A, B]

  def unright[A, B, C](p: P[Either[A, B], Either[A, C]]): P[B, C]
}
