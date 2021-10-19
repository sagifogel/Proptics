package proptics.applied

import proptics.applied.internal.{AppliedGetter1, AppliedReview0, AppliedTraversal0}
import proptics.{AppliedIso, Iso, Iso_}

trait AppliedIso_[S, T, A, B] extends AppliedTraversal0[S, T, A, B] with AppliedGetter1[S, A] with AppliedReview0[T, B] {
  val value: S
  val optic: Iso_[S, T, A, B]
}

object AppliedIso_ {
  def apply[S, T, A, B](_value: S, iso: Iso_[S, T, A, B]): AppliedIso_[S, T, A, B] =
    new AppliedIso_[S, T, A, B] {
      override val value: S = _value
      override val optic: Iso_[S, T, A, B] = iso
    }
}

object AppliedIso {
  def apply[S, A](_value: S, iso: Iso[S, A]): AppliedIso[S, A] =
    new AppliedIso[S, A] {
      override val value: S = _value
      override val optic: Iso[S, A] = iso
    }
}
