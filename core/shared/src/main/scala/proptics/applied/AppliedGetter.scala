package proptics.applied

import proptics.applied.internal.AppliedGetter1
import proptics.{AppliedGetter, Getter, Getter_, _}

trait AppliedGetter_[S, T, A, B] extends AppliedGetter1[S, A] {
  val value: S
  val optic: Getter_[S, T, A, B]

  /** compose this [[Getter_]] with a function lifted to a [[Getter_]] */
  final def focus[C, D](f: A => C): AppliedGetter_[S, T, C, D] = AppliedGetter_(value, optic.focus(f))

  /** compose this [[Getter_]] with an [[Iso_]], having this [[Getter_]] applied first */
  final def andThen[C, D](other: Iso_[A, B, C, D]): AppliedGetter_[S, T, C, D] = AppliedGetter_(value, optic.andThen(other))

  /** compose this [[Getter_]] with an [[AnIso_]], having this [[Getter_]] applied first */
  final def andThen[C, D](other: AnIso_[A, B, C, D]): AppliedGetter_[S, T, C, D] = AppliedGetter_(value, optic.andThen(other))

  /** compose this [[Getter_]] with a [[Lens_]], having this [[Getter_]] applied first */
  final def andThen[C, D](other: Lens_[A, B, C, D]): AppliedGetter_[S, T, C, D] = AppliedGetter_(value, optic.andThen(other))

  /** compose this [[Getter_]] with an [[ALens_]], having this [[Getter_]] applied first */
  final def andThen[C, D](other: ALens_[A, B, C, D]): AppliedGetter_[S, T, C, D] = AppliedGetter_(value, optic.andThen(other))

  /** compose this [[Getter_]] with a [[Prism_]], having this [[Getter_]] applied first */
  final def andThen[C, D](other: Prism_[A, B, C, D]): AppliedFold_[S, T, C, D] = AppliedFold_(value, optic.andThen(other))

  /** compose this [[Getter_]] with an [[APrism_]], having this [[Getter_]] applied first */
  final def andThen[C, D](other: APrism_[A, B, C, D]): AppliedFold_[S, T, C, D] = AppliedFold_(value, optic.andThen(other))

  /** compose this [[Getter_]] with an [[AffineTraversal_]], having this [[Getter_]] applied first */
  final def andThen[C, D](other: AffineTraversal_[A, B, C, D]): AppliedFold_[S, T, C, D] = AppliedFold_(value, optic.andThen(other))

  /** compose this [[Getter_]] with an [[AnAffineTraversal_]], having this [[Getter_]] applied first */
  final def andThen[C, D](other: AnAffineTraversal_[A, B, C, D]): AppliedFold_[S, T, C, D] = AppliedFold_(value, optic.andThen(other))

  /** compose this [[Getter_]] with a [[Traversal_]], having this [[Getter_]] applied first */
  final def andThen[C, D](other: Traversal_[A, B, C, D]): AppliedFold_[S, T, C, D] = AppliedFold_(value, optic.andThen(other))

  /** compose this [[Getter_]] with an [[ATraversal_]], having this [[Getter_]] applied first */
  final def andThen[C, D](other: ATraversal_[A, B, C, D]): AppliedFold_[S, T, C, D] = AppliedFold_(value, optic.andThen(other))

  /** compose this [[Getter_]] with a [[Getter_]], having this [[Getter_]] applied first */
  final def andThen[C, D](other: Getter_[A, B, C, D]): AppliedGetter_[S, T, C, D] = AppliedGetter_(value, optic.andThen(other))

  /** compose this [[Getter_]] with a [[Fold_]], having this [[Getter_]] applied first */
  final def andThen[C, D](other: Fold_[A, B, C, D]): AppliedFold_[S, T, C, D] = AppliedFold_(value, optic.andThen(other))
}

object AppliedGetter_ {
  def apply[S, T, A, B](_value: S, getter: Getter_[S, T, A, B]): AppliedGetter_[S, T, A, B] =
    new AppliedGetter_[S, T, A, B] {
      override val value: S = _value
      override val optic: Getter_[S, T, A, B] = getter
    }
}

object AppliedGetter {
  def apply[S, A](_value: S, getter: Getter[S, A]): AppliedGetter[S, A] =
    new AppliedGetter[S, A] {
      override val value: S = _value
      override val optic: Getter[S, A] = getter
    }
}
