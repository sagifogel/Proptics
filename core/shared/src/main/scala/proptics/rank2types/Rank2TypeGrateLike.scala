package proptics.rank2types

import proptics.profunctor.Closed

private[proptics] trait Rank2TypeGrateLike[S, T, A, B] {
  def apply[P[_, _]](pab: P[A, B])(implicit ev: Closed[P]): P[S, T]
}
