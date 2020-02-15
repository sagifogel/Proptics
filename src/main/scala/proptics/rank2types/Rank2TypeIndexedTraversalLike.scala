package proptics.rank2types

import proptics.internal.{Indexed, Wander}

trait Rank2TypeIndexedTraversalLike[I, S, T, A, B] {
  def apply[P[_, _]](index: Indexed[P, I, A, B])(implicit ev: Wander[P]): P[S, T]
}
