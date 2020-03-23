package proptics.rank2types

import proptics.internal.{Forget, Indexed}

trait Rank2TypeIndexedFoldLike[I, S, T, A, B] {
  def apply[R](indexed: Indexed[Forget[R, *, *], I, A, B]): Forget[R, S, T]
}
