package proptics.rank2types

import proptics.profunctor.Choice

trait Rank2TypePrismLike[S, T, A, B] {
  def apply[P[_, _]](pab: P[A, B])(implicit ev: Choice[P]): P[S, T]
}
