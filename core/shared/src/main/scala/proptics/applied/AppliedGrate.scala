package proptics.applied

import proptics._

trait AppliedGrate_[S, T, A, B] extends Serializable {
  val value: S
  val optic: Grate_[S, T, A, B]

  /** compose this [[Grate_]] with an [[Iso_]], having this [[Grate_]] applied first */
  final def andThen[C, D](other: Iso_[A, B, C, D]): AppliedGrate_[S, T, C, D] = AppliedGrate_(value, optic.andThen(other))

  /** compose this [[Grate_]] with an [[AnIso_]], having this [[Grate_]] applied first */
  final def andThen[C, D](other: AnIso_[A, B, C, D]): AppliedGrate_[S, T, C, D] = AppliedGrate_(value, optic.andThen(other))

  /** compose this [[Grate_]] with a [[Setter_]], having this [[Grate_]] applied first */
  final def andThen[C, D](other: Setter_[A, B, C, D]): AppliedSetter_[S, T, C, D] = AppliedSetter_(value, optic.andThen(other))

  /** compose this [[Grate_]] with a [[Grate_]], having this [[Grate_]] applied first */
  final def andThen[C, D](other: Grate_[A, B, C, D]): AppliedGrate_[S, T, C, D] = AppliedGrate_(value, optic.andThen(other))

  /** compose this [[Grate_]] with a [[Review_]], having this [[Grate_]] applied first */
  final def andThen[C, D](other: Review_[A, B, C, D]): AppliedReview_[S, T, C, D] = AppliedReview_(value, optic.andThen(other))
}

object AppliedGrate_ {
  def apply[S, T, A, B](_value: S, grate: Grate_[S, T, A, B]): AppliedGrate_[S, T, A, B] =
    new AppliedGrate_[S, T, A, B] {
      override val value: S = _value
      override val optic: Grate_[S, T, A, B] = grate
    }
}

object AppliedGrate {
  def apply[S, T, A, B](_value: S, grate: Grate[S, A]): AppliedGrate[S, A] =
    new AppliedGrate[S, A] {
      override val value: S = _value
      override val optic: Grate[S, A] = grate
    }
}
