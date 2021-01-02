package proptics.rank2types

import cats.arrow.Strong

private[proptics] trait Rank2TypeIndexedLensLike[I, S, T, A, B] {
  def apply[P[_, _]](piab: P[(A, I), B])(implicit ev: Strong[P]): P[S, T]
}
