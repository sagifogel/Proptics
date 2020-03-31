package proptics.rank2types

import cats.Monoid
import proptics.internal.{Forget, Indexed}

private[proptics] trait Rank2TypeIndexedFoldLike[I, S, T, A, B] {
  def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, A, B]): Forget[R, S, T]
}
