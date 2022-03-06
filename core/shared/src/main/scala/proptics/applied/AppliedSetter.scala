package proptics.applied

import proptics._
import proptics.applied.internal.AppliedSetter0

trait AppliedSetter_[S, T, A, B] extends AppliedSetter0[S, T, A, B] {
  val value: S
  val optic: Setter_[S, T, A, B]

  /** compose this [[Setter_]] with an [[Iso_]], having this [[Setter_]] applied first */
  final def andThen[C, D](other: Iso_[A, B, C, D]): AppliedSetter_[S, T, C, D] = AppliedSetter_(value, optic.andThen(other))

  /** compose this [[Setter_]] with an [[AnIso_]], having this [[Setter_]] applied first */
  final def andThen[C, D](other: AnIso_[A, B, C, D]): AppliedSetter_[S, T, C, D] = AppliedSetter_(value, optic.andThen(other))

  /** compose this [[Setter_]] with a [[Lens_]], having this [[Setter_]] applied first */
  final def andThen[C, D](other: Lens_[A, B, C, D]): AppliedSetter_[S, T, C, D] = AppliedSetter_(value, optic.andThen(other))

  /** compose this [[Setter_]] with an [[ALens_]], having this [[Setter_]] applied first */
  final def andThen[C, D](other: ALens_[A, B, C, D]): AppliedSetter_[S, T, C, D] = AppliedSetter_(value, optic.andThen(other))

  /** compose this [[Setter_]] with a [[Prism_]], having this [[Setter_]] applied first */
  final def andThen[C, D](other: Prism_[A, B, C, D]): AppliedSetter_[S, T, C, D] = AppliedSetter_(value, optic.andThen(other))

  /** compose this [[Setter_]] with an [[APrism_]], having this [[Setter_]] applied first */
  final def andThen[C, D](other: APrism_[A, B, C, D]): AppliedSetter_[S, T, C, D] = AppliedSetter_(value, optic.andThen(other))

  /** compose this [[Setter_]] with a [[AffineTraversal_]], having this [[Setter_]] applied first */
  final def andThen[C, D](other: AffineTraversal_[A, B, C, D]): AppliedSetter_[S, T, C, D] = AppliedSetter_(value, optic.andThen(other))

  /** compose this [[Setter_]] with an [[AnAffineTraversal_]], having this [[Setter_]] applied first */
  final def andThen[C, D](other: AnAffineTraversal_[A, B, C, D]): AppliedSetter_[S, T, C, D] = AppliedSetter_(value, optic.andThen(other))

  /** compose this [[Setter_]] with a [[Traversal_]], having this [[Setter_]] applied first */
  final def andThen[C, D](other: Traversal_[A, B, C, D]): AppliedSetter_[S, T, C, D] = AppliedSetter_(value, optic.andThen(other))

  /** compose this [[Setter_]] with an [[ATraversal_]], having this [[Setter_]] applied first */
  final def andThen[C, D](other: ATraversal_[A, B, C, D]): AppliedSetter_[S, T, C, D] = AppliedSetter_(value, optic.andThen(other))

  /** compose this [[Setter_]] with a [[Setter_]], having this [[Setter_]] applied first */
  final def andThen[C, D](other: Setter_[A, B, C, D]): AppliedSetter_[S, T, C, D] = AppliedSetter_(value, optic.andThen(other))

  /** compose this [[Setter_]] with a [[Grate_]], having this [[Setter_]] applied first */
  final def andThen[C, D](other: Grate_[A, B, C, D]): AppliedSetter_[S, T, C, D] = AppliedSetter_(value, optic.andThen(other))
}

object AppliedSetter_ {
  def apply[S, T, A, B](_value: S, setter: Setter_[S, T, A, B]): AppliedSetter_[S, T, A, B] =
    new AppliedSetter_[S, T, A, B] {
      override val value: S = _value
      override val optic: Setter_[S, T, A, B] = setter
    }
}

object AppliedSetter {
  def apply[S, A](_value: S, setter: Setter[S, A]): AppliedSetter[S, A] =
    new AppliedSetter[S, A] {
      override val value: S = _value
      override val optic: Setter[S, A] = setter
    }
}
