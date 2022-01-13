package proptics.applied

import proptics._
import proptics.applied.internal.{AppliedAffineTraversal0, AppliedReview0}

trait AppliedAPrism_[S, T, A, B] extends AppliedAffineTraversal0[S, T, A, B] with AppliedReview0[T, B] {
  val value: S
  val optic: APrism_[S, T, A, B]

  /** compose this [[APrism_]] with a function lifted to a [[Getter_]], having this [[Prism_]] applied first */
  final def focus[C, D](f: A => C): AppliedFold_[S, T, C, D] = AppliedFold_(value, optic.focus(f))

  /** compose this [[APrism_]] with an [[Iso_]], having this [[Prism_]] applied first */
  final def andThen[C, D](other: Iso_[A, B, C, D]): AppliedAPrism_[S, T, C, D] = AppliedAPrism_(value, optic.andThen(other))

  /** compose this [[APrism_]] with an [[AnIso_]], having this [[Prism_]] applied first */
  final def andThen[C, D](other: AnIso_[A, B, C, D]): AppliedAPrism_[S, T, C, D] = AppliedAPrism_(value, optic.andThen(other))

  /** compose this [[APrism_]] with a [[Lens_]], having this [[Prism_]] applied first */
  final def andThen[C, D](other: Lens_[A, B, C, D]): AppliedAffineTraversal_[S, T, C, D] = AppliedAffineTraversal_(value, optic.andThen(other))

  /** compose this [[APrism_]] with an [[ALens_]], having this [[Prism_]] applied first */
  final def andThen[C, D](other: ALens_[A, B, C, D]): AppliedAffineTraversal_[S, T, C, D] = AppliedAffineTraversal_(value, optic.andThen(other))

  /** compose this [[APrism_]] with a [[Prism_]], having this [[Prism_]] applied first */
  final def andThen[C, D](other: Prism_[A, B, C, D]): AppliedAPrism_[S, T, C, D] = AppliedAPrism_(value, optic.andThen(other))

  /** compose this [[APrism_]] with an [[APrism_]], having this [[Prism_]] applied first */
  final def andThen[C, D](other: APrism_[A, B, C, D]): AppliedAPrism_[S, T, C, D] = AppliedAPrism_(value, optic.andThen(other))

  final def andThen[C, D](other: AffineTraversal_[A, B, C, D]): AppliedAffineTraversal_[S, T, C, D] = AppliedAffineTraversal_(value, optic.andThen(other))

  /** compose this [[APrism_]] with an [[AffineTraversal_]], having this [[Prism_]] applied first */
  final def andThen[C, D](other: AnAffineTraversal_[A, B, C, D]): AppliedAnAffineTraversal_[S, T, C, D] = AppliedAnAffineTraversal_(value, optic.andThen(other))

  /** compose this [[APrism_]] with a [[Traversal_]], having this [[Prism_]] applied first */
  final def andThen[C, D](other: Traversal_[A, B, C, D]): AppliedTraversal_[S, T, C, D] = AppliedTraversal_(value, optic.andThen(other))

  /** compose this [[APrism_]] with an [[ATraversal_]], having this [[Prism_]] applied first */
  final def andThen[C, D](other: ATraversal_[A, B, C, D]): AppliedATraversal_[S, T, C, D] = AppliedATraversal_(value, optic.andThen(other))

  /** compose this [[APrism_]] with a [[Setter_]], having this [[Prism_]] applied first */
  final def andThen[C, D](other: Setter_[A, B, C, D]): AppliedSetter_[S, T, C, D] = AppliedSetter_(value, optic.andThen(other))

  /** compose this [[APrism_]] with a [[Getter_]], having this [[Prism_]] applied first */
  final def andThen[C, D](other: Getter_[A, B, C, D]): AppliedFold_[S, T, C, D] = AppliedFold_(value, optic.andThen(other))

  /** compose this [[APrism_]] with a [[Fold_]], having this [[Prism_]] applied first */
  final def andThen[C, D](other: Fold_[A, B, C, D]): AppliedFold_[S, T, C, D] = AppliedFold_(value, optic.andThen(other))

  /** compose this [[APrism_]] with a [[Review_]], having this [[Prism_]] applied first */
  final def andThen[C, D](other: Review_[A, B, C, D]): AppliedReview_[S, T, C, D] = AppliedReview_(value, optic.andThen(other))
}

object AppliedAPrism_ {
  def apply[S, T, A, B](_value: S, aPrism: APrism_[S, T, A, B]): AppliedAPrism_[S, T, A, B] =
    new AppliedAPrism_[S, T, A, B] {
      override val value: S = _value
      override val optic: APrism_[S, T, A, B] = aPrism
    }
}

object AppliedAPrism {
  def apply[S, A](_value: S, aPrism: APrism[S, A]): AppliedAPrism[S, A] =
    new AppliedAPrism[S, A] {
      override val value: S = _value
      override val optic: APrism[S, A] = aPrism
    }
}
