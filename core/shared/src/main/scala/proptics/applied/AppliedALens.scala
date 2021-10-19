package proptics.applied

import proptics.applied.internal.{AppliedGetter1, AppliedTraversal0}
import proptics.{ALens, ALens_, AppliedALens}

trait AppliedALens_[S, T, A, B] extends AppliedTraversal0[S, T, A, B] with AppliedGetter1[S, A] {
  val value: S
  val optic: ALens_[S, T, A, B]
}

object AppliedALens_ {
  def apply[S, T, A, B](_value: S, aLens: ALens_[S, T, A, B]): AppliedALens_[S, T, A, B] =
    new AppliedALens_[S, T, A, B] {
      override val value: S = _value
      override val optic: ALens_[S, T, A, B] = aLens
    }
}

object AppliedALens {
  def apply[S, A](_value: S, aLens: ALens[S, A]): AppliedALens[S, A] =
    new AppliedALens[S, A] {
      override val value: S = _value
      override val optic: ALens[S, A] = aLens
    }
}
