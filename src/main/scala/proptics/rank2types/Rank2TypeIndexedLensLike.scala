package proptics.rank2types

import cats.arrow.Strong

trait Rank2TypeIndexedLensLike[I, S, T, A, B] {
  def apply[P[_, _]](piab: P[(I, A), B])(implicit ev: Strong[P]): P[S, T]
}
