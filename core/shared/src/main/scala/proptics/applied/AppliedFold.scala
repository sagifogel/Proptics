package proptics.applied

import proptics._

trait AppliedFold_[S, T, A, B] extends AppliedFoldCompat[S, A] {
  val value: S
  val optic: Fold_[S, T, A, B]

  /** compose this [[Fold_]] with a function lifted to a [[Getter_]], having this [[Fold_]] applied first */
  final def focus[C](f: A => C): AppliedFold_[S, T, C, C] =
    AppliedFold_(value, optic.andThen(Getter_[A, B, C, C](f)))

  /** compose this [[Fold_]] with an [[AnIso_]], having this [[Fold_]] applied first */
  final def andThen[C, D](other: Iso_[A, B, C, D]): AppliedFold_[S, T, C, D] =
    AppliedFold_(value, optic.andThen(other))

  /** compose this [[Fold_]] with an [[AnIso_]], having this [[Fold_]] applied first */
  final def andThen[C, D](other: AnIso_[A, B, C, D]): AppliedFold_[S, T, C, D] =
    AppliedFold_(value, optic.andThen(other))

  /** compose this [[Fold_]] with a [[Lens_]], having this [[Fold_]] applied first */
  final def andThen[C, D](other: Lens_[A, B, C, D]): AppliedFold_[S, T, C, D] =
    AppliedFold_(value, optic.andThen(other))

  /** compose this [[Fold_]] with an [[ALens_]], having this [[Fold_]] applied first */
  final def andThen[C, D](other: ALens_[A, B, C, D]): AppliedFold_[S, T, C, D] =
    AppliedFold_(value, optic.andThen(other))

  /** compose this [[Fold_]] with a [[Prism_]], having this [[Fold_]] applied first */
  final def andThen[C, D](other: Prism_[A, B, C, D]): AppliedFold_[S, T, C, D] =
    AppliedFold_(value, optic.andThen(other))

  /** compose this [[Fold_]] with an [[APrism_]], having this [[Fold_]] applied first */
  final def andThen[C, D](other: APrism_[A, B, C, D]): AppliedFold_[S, T, C, D] =
    AppliedFold_(value, optic.andThen(other))

  /** compose this [[Fold_]] with a [[AffineTraversal_]], having this [[Fold_]] applied first */
  final def andThen[C, D](other: AffineTraversal_[A, B, C, D]): AppliedFold_[S, T, C, D] =
    AppliedFold_(value, optic.andThen(other))

  /** compose this [[Fold_]] with a [[AnAffineTraversal_]], having this [[Fold_]] applied first */
  final def andThen[C, D](other: AnAffineTraversal_[A, B, C, D]): AppliedFold_[S, T, C, D] =
    AppliedFold_(value, optic.andThen(other))

  /** compose this [[Fold_]] with a [[Traversal_]], having this [[Fold_]] applied first */
  final def andThen[C, D](other: Traversal_[A, B, C, D]): AppliedFold_[S, T, C, D] =
    AppliedFold_(value, optic.andThen(other))

  /** compose this [[Fold_]] with an [[ATraversal_]], having this [[Fold_]] applied first */
  final def andThen[C, D](other: ATraversal_[A, B, C, D]): AppliedFold_[S, T, C, D] =
    AppliedFold_(value, optic.andThen(other))

  /** compose this [[Fold_]] with a [[Getter_]], having this [[Fold_]] applied first */
  final def andThen[C, D](other: Getter_[A, B, C, D]): AppliedFold_[S, T, C, D] =
    AppliedFold_(value, optic.andThen(other))

  /** compose this [[Fold_]] with a [[Fold_]], having this [[Fold_]] applied first */
  final def andThen[C, D](other: Fold_[A, B, C, D]): AppliedFold_[S, T, C, D] =
    AppliedFold_(value, optic.andThen(other))
}

object AppliedFold_ {
  def apply[S, T, A, B](_value: S, _fold: Fold_[S, T, A, B]): AppliedFold_[S, T, A, B] =
    new AppliedFold_[S, T, A, B] {
      override val value: S = _value
      override val optic: Fold_[S, T, A, B] = _fold
    }
}

object AppliedFold {
  def apply[S, A](_value: S, _fold: Fold[S, A]): AppliedFold[S, A] =
    new AppliedFold[S, A] {
      override val value: S = _value
      override val optic: Fold[S, A] = _fold
    }
}
