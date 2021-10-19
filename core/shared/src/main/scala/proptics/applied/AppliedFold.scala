package proptics.applied

import proptics.{AppliedFold, Fold, Fold_}

trait AppliedFold_[S, T, A, B] extends AppliedFoldCompat[S, A] {
  val value: S
  val optic: Fold_[S, T, A, B]
}

object AppliedFold_ {
  def apply[S, T, A, B](_value: S, setter: Fold_[S, T, A, B]): AppliedFold_[S, T, A, B] =
    new AppliedFold_[S, T, A, B] {
      override val value: S = _value
      override val optic: Fold_[S, T, A, B] = setter
    }
}

object AppliedFold {
  def apply[S, A](_value: S, _fold: Fold[S, A]): AppliedFold[S, A] =
    new AppliedFold[S, A] {
      override val value: S = _value
      override val optic: Fold[S, A] = _fold
    }
}
