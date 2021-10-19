package proptics.applied

import proptics.applied.internal.AppliedReview0
import proptics.{AppliedReview, Review, Review_}

trait AppliedReview_[S, T, A, B] extends AppliedReview0[T, B] {
  val value: S
  val optic: Review_[S, T, A, B]
}

object AppliedReview_ {
  def apply[S, T, A, B](_value: S, _review: Review_[S, T, A, B]): AppliedReview_[S, T, A, B] =
    new AppliedReview_[S, T, A, B] {
      override val value: S = _value
      override val optic: Review_[S, T, A, B] = _review
    }
}

object AppliedReview {
  def apply[S, A](_value: S, _review: Review[S, A]): AppliedReview[S, A] =
    new AppliedReview[S, A] {
      override val value: S = _value
      override val optic: Review[S, A] = _review
    }
}
