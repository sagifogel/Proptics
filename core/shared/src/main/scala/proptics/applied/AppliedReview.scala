package proptics.applied

import proptics._
import proptics.applied.internal.AppliedReview0

trait AppliedReview_[S, T, A, B] extends AppliedReview0[T, B] {
  val value: S
  val optic: Review_[S, T, A, B]

  /** compose this [[Review_]] with an [[Iso_]], having this [[Review_]] applied first */
  final def andThen[C, D](other: Iso_[A, B, C, D]): AppliedReview_[S, T, C, D] = AppliedReview_(value, optic.andThen(other))

  /** compose this [[Review_]] with an [[AnIso_]], having this [[Review_]] applied first */
  final def andThen[C, D](other: AnIso_[A, B, C, D]): AppliedReview_[S, T, C, D] = AppliedReview_(value, optic.andThen(other))

  /** compose this [[Review_]] with a [[Prism_]], having this [[Review_]] applied first */
  final def andThen[C, D](other: Prism_[A, B, C, D]): AppliedReview_[S, T, C, D] = AppliedReview_(value, optic.andThen(other))

  /** compose this [[Review_]] with an [[APrism_]], having this [[Review_]] applied first */
  final def andThen[C, D](other: APrism_[A, B, C, D]): AppliedReview_[S, T, C, D] = AppliedReview_(value, optic.andThen(other))

  /** compose this [[Review_]] with a [[Grate_]], having this [[Review_]] applied first */
  final def andThen[C, D](other: Grate_[A, B, C, D]): AppliedReview_[S, T, C, D] = AppliedReview_(value, optic.andThen(other))

  /** compose this [[Review_]] with a [[Review_]], having this [[Review_]] applied first */
  final def andThen[C, D](other: Review_[A, B, C, D]): AppliedReview_[S, T, C, D] = AppliedReview_(value, optic.andThen(other))
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
