package proptics.rank2types

import cats.arrow.Strong
import proptics.profunctor.Choice

private[proptics] trait Rank2TypeAffineTraversalLike[S, T, A, B] {
  def apply[P[_, _]](pab: P[A, B])(implicit ev0: Choice[P], ev1: Strong[P]): P[S, T]
}
