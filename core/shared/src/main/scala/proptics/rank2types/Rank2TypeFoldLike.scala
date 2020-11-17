package proptics.rank2types

import cats.Monoid

import proptics.internal.Forget

private[proptics] trait Rank2TypeFoldLike[S, T, A, B] {
  def apply[R: Monoid](forget: Forget[R, A, B]): Forget[R, S, T]
}
