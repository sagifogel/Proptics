package proptics.applied

import proptics.applied.internal.{AppliedGetter1, AppliedReview0, AppliedTraversal0}
import proptics.{AnIso, AnIso_, AppliedAnIso}

trait AppliedAnIso_[S, T, A, B] extends AppliedTraversal0[S, T, A, B] with AppliedGetter1[S, A] with AppliedReview0[T, B] {
  val value: S
  val optic: AnIso_[S, T, A, B]
}

object AppliedAnIso_ {
  def apply[S, T, A, B](_value: S, anIso: AnIso_[S, T, A, B]): AppliedAnIso_[S, T, A, B] =
    new AppliedAnIso_[S, T, A, B] {
      override val value: S = _value
      override val optic: AnIso_[S, T, A, B] = anIso
    }
}

object AppliedAnIso {
  def apply[S, A](_value: S, anIso: AnIso[S, A]): AppliedAnIso[S, A] =
    new AppliedAnIso[S, A] {
      override val value: S = _value
      override val optic: AnIso[S, A] = anIso
    }
}
