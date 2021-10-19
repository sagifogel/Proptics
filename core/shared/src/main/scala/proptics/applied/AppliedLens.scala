package proptics.applied

import proptics.applied.internal.{AppliedGetter1, AppliedTraversal0}
import proptics.{AppliedLens, Lens, Lens_}

trait AppliedLens_[S, T, A, B] extends AppliedTraversal0[S, T, A, B] with AppliedGetter1[S, A] {
  val value: S
  val optic: Lens_[S, T, A, B]
}

object AppliedLens_ {
  def apply[S, T, A, B](_value: S, lens: Lens_[S, T, A, B]): AppliedLens_[S, T, A, B] =
    new AppliedLens_[S, T, A, B] {
      override val value: S = _value
      override val optic: Lens_[S, T, A, B] = lens
    }
}

object AppliedLens {
  def apply[S, A](_value: S, lens: Lens[S, A]): AppliedLens[S, A] =
    new AppliedLens_[S, S, A, A] {
      override val value: S = _value
      override val optic: Lens[S, A] = lens
    }
}
