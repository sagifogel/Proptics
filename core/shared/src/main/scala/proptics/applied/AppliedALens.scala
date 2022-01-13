package proptics.applied

import proptics._
import proptics.applied.internal.{AppliedGetter1, AppliedTraversal0}

trait AppliedALens_[S, T, A, B] extends AppliedTraversal0[S, T, A, B] with AppliedGetter1[S, A] {
  val value: S
  val optic: ALens_[S, T, A, B]

  /** compose this [[ALens_]] with a function lifted to a [[Getter_]], having this [[Lens_]] applied first */
  final def focus[C, D](f: A => C): AppliedGetter_[S, T, C, D] = AppliedGetter_(value, optic.focus(f))

  /** compose this [[ALens_]] with an [[Iso_]], having this [[Lens_]] applied first */
  final def andThen[C, D](other: Iso_[A, B, C, D]): AppliedLens_[S, T, C, D] = AppliedLens_(value, optic.andThen(other))

  /** compose this [[ALens_]] with an [[AnIso_]], having this [[Lens_]] applied first */
  final def andThen[C, D](other: AnIso_[A, B, C, D]): AppliedLens_[S, T, C, D] = AppliedLens_(value, optic.andThen(other))

  /** compose this [[ALens_]] with a [[Lens_]], having this [[Lens_]] applied first */
  final def andThen[C, D](other: Lens_[A, B, C, D]): AppliedLens_[S, T, C, D] = AppliedLens_(value, optic.andThen(other))

  /** compose this [[ALens_]] with an [[ALens_]], having this [[Lens_]] applied first */
  final def andThen[C, D](other: ALens_[A, B, C, D]): AppliedLens_[S, T, C, D] = AppliedLens_(value, optic.andThen(other))

  /** compose this [[ALens_]] with a [[Prism_]], having this [[Lens_]] applied first */
  final def andThen[C, D](other: Prism_[A, B, C, D]): AppliedAffineTraversal_[S, T, C, D] = AppliedAffineTraversal_(value, optic.andThen(other))

  /** compose this [[ALens_]] with an [[APrism_]], having this [[Lens_]] applied first */
  final def andThen[C, D](other: APrism_[A, B, C, D]): AppliedAffineTraversal_[S, T, C, D] = AppliedAffineTraversal_(value, optic.andThen(other))

  /** compose this [[ALens_]] with a [[AffineTraversal_]], having this [[Lens_]] applied first */
  final def andThen[C, D](other: AffineTraversal_[A, B, C, D]): AppliedAffineTraversal_[S, T, C, D] = AppliedAffineTraversal_(value, optic.andThen(other))

  /** compose this [[ALens_]] with an [[AnAffineTraversal_]], having this [[Lens_]] applied first */
  final def andThen[C, D](other: AnAffineTraversal_[A, B, C, D]): AppliedAnAffineTraversal_[S, T, C, D] = AppliedAnAffineTraversal_(value, optic.andThen(other))

  /** compose this [[ALens_]] with a [[Traversal_]], having this [[Lens_]] applied first */
  final def andThen[C, D](other: Traversal_[A, B, C, D]): AppliedTraversal_[S, T, C, D] = AppliedTraversal_(value, optic.andThen(other))

  /** compose this [[ALens_]] with an [[ATraversal_]], having this [[Lens_]] applied first */
  final def andThen[C, D](other: ATraversal_[A, B, C, D]): AppliedATraversal_[S, T, C, D] = AppliedATraversal_(value, optic.andThen(other))

  /** compose this [[ALens_]] with a [[Setter_]], having this [[Lens_]] applied first */
  final def andThen[C, D](other: Setter_[A, B, C, D]): AppliedSetter_[S, T, C, D] = AppliedSetter_(value, optic.andThen(other))

  /** compose this [[ALens_]] with a [[Getter_]], having this [[Lens_]] applied first */
  final def andThen[C, D](other: Getter_[A, B, C, D]): AppliedGetter_[S, T, C, D] = AppliedGetter_(value, optic.andThen(other))

  /** compose this [[ALens_]] with a [[Fold_]], having this [[Lens_]] applied first */
  final def andThen[C, D](other: Fold_[A, B, C, D]): AppliedFold_[S, T, C, D] = AppliedFold_(value, optic.andThen(other))
}

object AppliedALens_ {
  def apply[S, T, A, B](_value: S, aLens: ALens_[S, T, A, B]): AppliedALens_[S, T, A, B] =
    new AppliedALens_[S, T, A, B] {
      override val value: S = _value
      override val optic: ALens_[S, T, A, B] = aLens
    }
}

object AppliedALens {
  def apply[S, A](_value: S, aLens: ALens[S, A]): AppliedALens[S, A] =
    new AppliedALens[S, A] {
      override val value: S = _value
      override val optic: ALens[S, A] = aLens
    }
}
