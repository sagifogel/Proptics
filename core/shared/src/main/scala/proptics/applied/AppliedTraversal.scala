package proptics.applied

import cats.data.State

import proptics._
import proptics.applied.internal._
import proptics.internal.{Bazaar, Sellable}
import proptics.profunctor.Corepresentable.Aux

trait AppliedTraversal_[S, T, A, B] extends AppliedTraversal1[S, T, A, B] {
  val value: S
  val optic: Traversal_[S, T, A, B]

  /** convert a [[Traversal_]] into a [[Lens_]] over a list of the [[Traversal_]]'s foci */
  final def unsafePartsOf(implicit ev0: Sellable[* => *, Bazaar[* => *, *, *, Unit, *]], ev1: Aux[* => *, State[List[B], *]]): AppliedLens_[S, T, List[A], List[B]] =
    AppliedLens_(value, optic.unsafePartsOf)

  /** compose this [[Traversal_]] with a function lifted to a [[Getter_]], having this [[Traversal_]] applied first */
  final def focus[C, D](f: A => C): AppliedFold_[S, T, C, D] = AppliedFold_(value, optic.focus(f))

  /** compose this [[Traversal_]] with an [[Iso_]], having this [[Traversal_]] applied first */
  final def andThen[C, D](other: Iso_[A, B, C, D]): AppliedTraversal_[S, T, C, D] = AppliedTraversal_[S, T, C, D](value, optic.andThen(other))

  /** compose this [[Traversal_]] with an [[AnIso_]], having this [[Traversal_]] applied first */
  final def andThen[C, D](other: AnIso_[A, B, C, D]): AppliedTraversal_[S, T, C, D] = AppliedTraversal_[S, T, C, D](value, optic.andThen(other))

  /** compose this [[Traversal_]] with a [[Lens_]], having this [[Traversal_]] applied first */
  final def andThen[C, D](other: Lens_[A, B, C, D]): AppliedTraversal_[S, T, C, D] = AppliedTraversal_[S, T, C, D](value, optic.andThen(other))

  /** compose this [[Traversal_]] with an [[ALens_]], having this [[Traversal_]] applied first */
  final def andThen[C, D](other: ALens_[A, B, C, D]): AppliedTraversal_[S, T, C, D] = AppliedTraversal_[S, T, C, D](value, optic.andThen(other))

  /** compose this [[Traversal_]] with a [[Prism_]], having this [[Traversal_]] applied first */
  final def andThen[C, D](other: Prism_[A, B, C, D]): AppliedTraversal_[S, T, C, D] = AppliedTraversal_[S, T, C, D](value, optic.andThen(other))

  /** compose this [[Traversal_]] with an [[APrism_]], having this [[Traversal_]] applied first */
  final def andThen[C, D](other: APrism_[A, B, C, D]): AppliedTraversal_[S, T, C, D] = AppliedTraversal_[S, T, C, D](value, optic.andThen(other))

  /** compose this [[Traversal_]] with an [[AffineTraversal_]], having this [[Traversal_]] applied first */
  final def andThen[C, D](other: AffineTraversal_[A, B, C, D]): AppliedTraversal_[S, T, C, D] = AppliedTraversal_[S, T, C, D](value, optic.andThen(other))

  /** compose this [[Traversal_]] with an [[AnAffineTraversal_]], having this [[Traversal_]] applied first */
  final def andThen[C, D](other: AnAffineTraversal_[A, B, C, D]): AppliedTraversal_[S, T, C, D] = AppliedTraversal_[S, T, C, D](value, optic.andThen(other))

  /** compose this [[Traversal_]] with a [[Traversal_]], having this [[Traversal_]] applied first */
  final def andThen[C, D](other: Traversal_[A, B, C, D]): AppliedTraversal_[S, T, C, D] = AppliedTraversal_[S, T, C, D](value, optic.andThen(other))

  /** compose this [[Traversal_]] with an [[ATraversal_]], having this [[Traversal_]] applied first */
  final def andThen[C, D](other: ATraversal_[A, B, C, D]): AppliedATraversal_[S, T, C, D] = AppliedATraversal_[S, T, C, D](value, optic.andThen(other))

  /** compose this [[Traversal_]] with a [[Setter_]], having this [[Traversal_]] applied first */
  final def andThen[C, D](other: Setter_[A, B, C, D]): AppliedSetter_[S, T, C, D] = AppliedSetter_(value, optic.andThen(other))

  /** compose this [[Traversal_]] with a [[Getter_]], having this [[Traversal_]] applied first */
  final def andThen[C, D](other: Getter_[A, B, C, D]): AppliedFold_[S, T, C, D] = AppliedFold_(value, optic.andThen(other))

  /** compose this [[Traversal_]] with a [[Fold_]], having this [[Traversal_]] applied first */
  final def andThen[C, D](other: Fold_[A, B, C, D]): AppliedFold_[S, T, C, D] = AppliedFold_(value, optic.andThen(other))
}

object AppliedTraversal_ {
  def apply[S, T, A, B](_value: S, traversal: Traversal_[S, T, A, B]): AppliedTraversal_[S, T, A, B] =
    new AppliedTraversal_[S, T, A, B] {
      override val value: S = _value
      override val optic: Traversal_[S, T, A, B] = traversal
    }
}

object AppliedTraversal {
  def apply[S, A](_value: S, traversal: Traversal[S, A]): AppliedTraversal[S, A] =
    new AppliedTraversal[S, A] {
      override val value: S = _value
      override val optic: Traversal[S, A] = traversal
    }
}
