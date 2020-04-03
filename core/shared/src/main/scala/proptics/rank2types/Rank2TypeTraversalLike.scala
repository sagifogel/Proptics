package proptics.rank2types

import proptics.internal.Wander

private[proptics] trait Rank2TypeTraversalLike[S, T, A, B] {
  def apply[P[_, _]](pab: P[A, B])(implicit ev: Wander[P]): P[S, T]
}
