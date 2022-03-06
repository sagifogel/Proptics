package proptics.applied

import proptics._
import proptics.applied.internal.{AppliedGetter1, AppliedReview0, AppliedTraversal0}

trait AppliedIso_[S, T, A, B] extends AppliedTraversal0[S, T, A, B] with AppliedGetter1[S, A] with AppliedReview0[T, B] {
  val value: S
  val optic: Iso_[S, T, A, B]

  /** compose this [[Iso_]] with a function lifted to a [[Getter_]], having this [[Iso_]] applied first */
  final def focus[C, D](f: A => C): AppliedGetter_[S, T, C, D] = AppliedGetter_(value, optic.focus(f))

  /** compose this [[Iso_]] with an [[Iso_]], having this [[Iso_]] applied first */
  final def andThen[C, D](other: Iso_[A, B, C, D]): AppliedIso_[S, T, C, D] = AppliedIso_(value, optic.andThen(other))

  /** compose this [[Iso_]] with an [[AnIso_]], having this [[Iso_]] applied first */
  final def andThen[C, D](other: AnIso_[A, B, C, D]): AppliedAnIso_[S, T, C, D] = AppliedAnIso_(value, optic.andThen(other))

  /** compose this [[Iso_]] with a [[Lens_]], having this [[Iso_]] applied first */
  final def andThen[C, D](other: Lens_[A, B, C, D]): AppliedLens_[S, T, C, D] = AppliedLens_(value, optic.andThen(other))

  /** compose this [[Iso_]] with an [[ALens_]], having this [[Iso_]] applied first */
  final def andThen[C, D](other: ALens_[A, B, C, D]): AppliedALens_[S, T, C, D] = AppliedALens_(value, optic.andThen(other))

  /** compose this [[Iso_]] with a [[Prism_]], having this [[Iso_]] applied first */
  final def andThen[C, D](other: Prism_[A, B, C, D]): AppliedPrism_[S, T, C, D] = AppliedPrism_(value, optic.andThen(other))

  /** compose this [[Iso_]] with an [[APrism_]], having this [[Iso_]] applied first */
  final def andThen[C, D](other: APrism_[A, B, C, D]): AppliedAPrism_[S, T, C, D] = AppliedAPrism_(value, optic.andThen(other))

  /** compose this [[Iso_]] with an [[AffineTraversal_]], having this [[Iso_]] applied first */
  final def andThen[C, D](other: AffineTraversal_[A, B, C, D]): AppliedAffineTraversal_[S, T, C, D] = AppliedAffineTraversal_(value, optic.andThen(other))

  /** compose this [[Iso_]] with an [[AnAffineTraversal_]], having this [[Iso_]] applied first */
  final def andThen[C, D](other: AnAffineTraversal_[A, B, C, D]): AppliedAnAffineTraversal_[S, T, C, D] = AppliedAnAffineTraversal_(value, optic.andThen(other))

  /** compose this [[Iso_]] with a [[Traversal_]], having this [[Iso_]] applied first */
  final def andThen[C, D](other: Traversal_[A, B, C, D]): AppliedTraversal_[S, T, C, D] = AppliedTraversal_(value, optic.andThen(other))

  /** compose this [[Iso_]] with an [[ATraversal_]], having this [[Iso_]] applied first */
  final def andThen[C, D](other: ATraversal_[A, B, C, D]): AppliedATraversal_[S, T, C, D] = AppliedATraversal_(value, optic.andThen(other))

  /** compose this [[Iso_]] with a [[Setter_]], having this [[Iso_]] applied first */
  final def andThen[C, D](other: Setter_[A, B, C, D]): AppliedSetter_[S, T, C, D] = AppliedSetter_(value, optic.andThen(other))

  /** compose this [[Iso_]] with a [[Getter_]], having this [[Iso_]] applied first */
  final def andThen[C, D](other: Getter_[A, B, C, D]): AppliedGetter_[S, T, C, D] = AppliedGetter_(value, optic.andThen(other))

  /** compose this [[Iso_]] with a [[Fold_]], having this [[Iso_]] applied first */
  final def andThen[C, D](other: Fold_[A, B, C, D]): AppliedFold_[S, T, C, D] = AppliedFold_(value, optic.andThen(other))

  /** compose this [[Iso_]] with a [[Grate_]], having this [[Iso_]] applied first */
  final def andThen[C, D](other: Grate_[A, B, C, D]): AppliedGrate_[S, T, C, D] = AppliedGrate_(value, optic.andThen(other))

  /** compose this [[Iso_]] with a [[Review_]], having this [[Iso_]] applied first */
  final def andThen[C, D](other: Review_[A, B, C, D]): AppliedReview_[S, T, C, D] = AppliedReview_(value, optic.andThen(other))
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
