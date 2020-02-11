package proptics.rank2types

import cats.arrow.Profunctor

trait Rank2TypeIsoLike[S, T, A, B] {
  def apply[P[_, _]](pab: P[A, B])(implicit ev: Profunctor[P]): P[S, T]
}
