package proptics.applied

import proptics._
import proptics.applied.internal.AppliedAffineTraversal0

trait AppliedAnAffineTraversal_[S, T, A, B] extends AppliedAffineTraversal0[S, T, A, B] {
  val value: S
  val optic: AnAffineTraversal_[S, T, A, B]

  /** compose this [[AnAffineTraversal_]] with a function lifted to a [[Getter_]], having this [[AnAffineTraversal_]] applied first */
  final def focus[C, D](f: A => C): AppliedFold_[S, T, C, D] = AppliedFold_(value, optic.focus(f))

  /** compose this [[AnAffineTraversal_]] with an [[Iso_]], having this [[AnAffineTraversal_]] applied first */
  final def andThen[C, D](other: Iso_[A, B, C, D]): AppliedAnAffineTraversal_[S, T, C, D] = AppliedAnAffineTraversal_(value, optic.andThen(other))

  /** compose this [[AnAffineTraversal_]] with an [[AnIso_]], having this [[AnAffineTraversal_]] applied first */
  final def andThen[C, D](other: AnIso_[A, B, C, D]): AppliedAnAffineTraversal_[S, T, C, D] = AppliedAnAffineTraversal_(value, optic.andThen(other))

  /** compose this [[AnAffineTraversal_]] with a [[Lens_]], having this [[AnAffineTraversal_]] applied first */
  final def andThen[C, D](other: Lens_[A, B, C, D]): AppliedAnAffineTraversal_[S, T, C, D] = AppliedAnAffineTraversal_(value, optic.andThen(other))

  /** compose this [[AnAffineTraversal_]] with an [[ALens_]], having this [[AnAffineTraversal_]] applied first */
  final def andThen[C, D](other: ALens_[A, B, C, D]): AppliedAnAffineTraversal_[S, T, C, D] = AppliedAnAffineTraversal_(value, optic.andThen(other))

  /** compose this [[AnAffineTraversal_]] with a [[Prism_]], having this [[AnAffineTraversal_]] applied first */
  final def andThen[C, D](other: Prism_[A, B, C, D]): AppliedAnAffineTraversal_[S, T, C, D] = AppliedAnAffineTraversal_(value, optic.andThen(other))

  final def andThen[C, D](other: APrism_[A, B, C, D]): AppliedAnAffineTraversal_[S, T, C, D] = AppliedAnAffineTraversal_(value, optic.andThen(other))

  /** compose this [[AnAffineTraversal_]] with an [[AffineTraversal_]], having this [[AnAffineTraversal_]] applied first */
  final def andThen[C, D](other: AffineTraversal_[A, B, C, D]): AppliedAnAffineTraversal_[S, T, C, D] = AppliedAnAffineTraversal_(value, optic.andThen(other))

  /** compose this [[AnAffineTraversal_]] with an [[AnAffineTraversal_]], having this [[AnAffineTraversal_]] applied first */
  final def andThen[C, D](other: AnAffineTraversal_[A, B, C, D]): AppliedAnAffineTraversal_[S, T, C, D] = AppliedAnAffineTraversal_(value, optic.andThen(other))

  /** compose this [[AnAffineTraversal_]] with a [[Traversal_]], having this [[AnAffineTraversal_]] applied first */
  final def andThen[C, D](other: Traversal_[A, B, C, D]): AppliedTraversal_[S, T, C, D] = AppliedTraversal_(value, optic.andThen(other))

  /** compose this [[AnAffineTraversal_]] with an [[ATraversal_]], having this [[AnAffineTraversal_]] applied first */
  final def andThen[C, D](other: ATraversal_[A, B, C, D]): AppliedATraversal_[S, T, C, D] = AppliedATraversal_(value, optic.andThen(other))

  /** compose this [[AnAffineTraversal_]] with a [[Setter_]], having this [[AnAffineTraversal_]] applied first */
  final def andThen[C, D](other: Setter_[A, B, C, D]): AppliedSetter_[S, T, C, D] = AppliedSetter_(value, optic.andThen(other))

  /** compose this [[AnAffineTraversal_]] with a [[Getter_]], having this [[AnAffineTraversal_]] applied first */
  final def andThen[C, D](other: Getter_[A, B, C, D]): AppliedFold_[S, T, C, D] = AppliedFold_(value, optic.andThen(other))

  /** compose this [[AnAffineTraversal_]] with a [[Fold_]], having this [[AnAffineTraversal_]] applied first */
  final def andThen[C, D](other: Fold_[A, B, C, D]): AppliedFold_[S, T, C, D] = AppliedFold_(value, optic.andThen(other))
}

object AppliedAnAffineTraversal_ {
  def apply[S, T, A, B](_value: S, anAffineTraversal: AnAffineTraversal_[S, T, A, B]): AppliedAnAffineTraversal_[S, T, A, B] =
    new AppliedAnAffineTraversal_[S, T, A, B] {
      override val value: S = _value
      override val optic: AnAffineTraversal_[S, T, A, B] = anAffineTraversal
    }
}

object AppliedAnAffineTraversal {
  def apply[S, A](_value: S, anAffineTraversal: AnAffineTraversal[S, A]): AppliedAnAffineTraversal[S, A] =
    new AppliedAnAffineTraversal[S, A] {
      override val value: S = _value
      override val optic: AnAffineTraversal[S, A] = anAffineTraversal
    }
}
