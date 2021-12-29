package proptics.applied

import proptics._
import proptics.applied.internal.AppliedAffineTraversal0

trait AppliedAffineTraversal_[S, T, A, B] extends AppliedAffineTraversal0[S, T, A, B] {
  val value: S
  val optic: AffineTraversal_[S, T, A, B]

  /** compose this [[AffineTraversal_]] with a function lifted to a [[Getter_]], having this [[AffineTraversal_]] applied first */
  final def focus[C, D](f: A => C): AppliedFold_[S, T, C, D] = AppliedFold_(value, optic.andThen(Getter_[A, B, C, D](f)))

  /** compose this [[AffineTraversal_]] with an [[Iso_]], having this [[AffineTraversal_]] applied first */
  final def andThen[C, D](other: Iso_[A, B, C, D]): AppliedAffineTraversal_[S, T, C, D] = AppliedAffineTraversal_(value, optic.andThen(other))

  /** compose this [[AffineTraversal_]] with an [[AnIso_]], having this [[AffineTraversal_]] applied first */
  final def andThen[C, D](other: AnIso_[A, B, C, D]): AppliedAffineTraversal_[S, T, C, D] = AppliedAffineTraversal_(value, optic.andThen(other))

  /** compose this [[AffineTraversal_]] with a [[Lens_]], having this [[AffineTraversal_]] applied first */
  final def andThen[C, D](other: Lens_[A, B, C, D]): AppliedAffineTraversal_[S, T, C, D] = AppliedAffineTraversal_(value, optic.andThen(other))

  /** compose this [[AffineTraversal_]] with an [[ALens_]], having this [[AffineTraversal_]] applied first */
  final def andThen[C, D](other: ALens_[A, B, C, D]): AppliedAffineTraversal_[S, T, C, D] = AppliedAffineTraversal_(value, optic.andThen(other))

  /** compose this [[AffineTraversal_]] with a [[Prism_]], having this [[AffineTraversal_]] applied first */
  final def andThen[C, D](other: Prism_[A, B, C, D]): AppliedAffineTraversal_[S, T, C, D] = AppliedAffineTraversal_(value, optic.andThen(other))

  /** compose this [[AffineTraversal_]] with an [[APrism_]], having this [[AffineTraversal_]] applied first */
  final def andThen[C, D](other: APrism_[A, B, C, D]): AppliedAffineTraversal_[S, T, C, D] = AppliedAffineTraversal_(value, optic.andThen(other))

  /** compose this [[AffineTraversal_]] with an [[AffineTraversal_]], having this [[AffineTraversal_]] applied first */
  final def andThen[C, D](other: AffineTraversal_[A, B, C, D]): AppliedAffineTraversal_[S, T, C, D] = AppliedAffineTraversal_(value, optic.andThen(other))

  /** compose this [[AffineTraversal_]] with an [[AnAffineTraversal_]], having this [[AffineTraversal_]] applied first */
  final def andThen[C, D](other: AnAffineTraversal_[A, B, C, D]): AppliedAffineTraversal_[S, T, C, D] = AppliedAffineTraversal_(value, optic.andThen(other))

  /** compose this [[AffineTraversal_]] with a [[Traversal_]], having this [[AffineTraversal_]] applied first */
  final def andThen[C, D](other: Traversal_[A, B, C, D]): AppliedTraversal_[S, T, C, D] = AppliedTraversal_(value, optic.andThen(other))

  /** compose this [[AffineTraversal_]] with an [[ATraversal_]], having this [[AffineTraversal_]] applied first */
  final def andThen[C, D](other: ATraversal_[A, B, C, D]): AppliedATraversal_[S, T, C, D] = AppliedATraversal_(value, optic.andThen(other))

  /** compose this [[AffineTraversal_]] with a [[Setter_]], having this [[AffineTraversal_]] applied first */
  final def andThen[C, D](other: Setter_[A, B, C, D]): AppliedSetter_[S, T, C, D] = AppliedSetter_(value, optic.andThen(other))

  /** compose this [[AffineTraversal_]] with a [[Getter_]], having this [[AffineTraversal_]] applied first */
  final def andThen[C, D](other: Getter_[A, B, C, D]): AppliedFold_[S, T, C, D] = AppliedFold_(value, optic.andThen(other))

  /** compose this [[AffineTraversal_]] with a [[Fold_]], having this [[AffineTraversal_]] applied first */
  final def andThen[C, D](other: Fold_[A, B, C, D]): AppliedFold_[S, T, C, D] = AppliedFold_(value, optic.andThen(other))
}

object AppliedAffineTraversal_ {
  def apply[S, T, A, B](_value: S, affineTraversal: AffineTraversal_[S, T, A, B]): AppliedAffineTraversal_[S, T, A, B] =
    new AppliedAffineTraversal_[S, T, A, B] {
      override val value: S = _value
      override val optic: AffineTraversal_[S, T, A, B] = affineTraversal
    }
}

object AppliedAffineTraversal {
  def apply[S, A](_value: S, affineTraversal: AffineTraversal[S, A]): AppliedAffineTraversal[S, A] =
    new AppliedAffineTraversal[S, A] {
      override val value: S = _value
      override val optic: AffineTraversal[S, A] = affineTraversal
    }
}
