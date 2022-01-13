package proptics.applied

import cats.data.State

import proptics._
import proptics.applied.internal.AppliedTraversal1
import proptics.internal.{Bazaar, Sellable}
import proptics.profunctor.Corepresentable.Aux

private[proptics] trait AppliedATraversal_[S, T, A, B] extends AppliedTraversal1[S, T, A, B] {
  val value: S
  val optic: ATraversal_[S, T, A, B]

  /** convert a [[ATraversal]] into a [[Lens]] over a list of the [[ATraversal]]'s foci */
  final def unsafePartsOf(implicit ev0: Sellable[* => *, Bazaar[* => *, *, *, Unit, *]], ev1: Aux[* => *, State[List[B], *]]): AppliedLens_[S, T, List[A], List[B]] =
    AppliedLens_(value, optic.unsafePartsOf)

  /** compose this [[ATraversal_]] with a function lifted to a [[Getter_]], having this [[ATraversal_]] applied first */
  final def focus[C, D](f: A => C): AppliedFold_[S, T, C, D] = AppliedFold_(value, optic.focus(f))

  /** compose this [[ATraversal_]] with an [[Iso_]], having this [[ATraversal_]] applied first */
  final def andThen[C, D](other: Iso_[A, B, C, D]): AppliedATraversal_[S, T, C, D] = AppliedATraversal_(value, optic.andThen(other))

  /** compose this [[ATraversal_]] with an [[AnIso_]], having this [[ATraversal_]] applied first */
  final def andThen[C, D](other: AnIso_[A, B, C, D]): AppliedATraversal_[S, T, C, D] = AppliedATraversal_(value, optic.andThen(other))

  /** compose this [[ATraversal_]] with a [[Lens_]], having this [[ATraversal_]] applied first */
  final def andThen[C, D](other: Lens_[A, B, C, D]): AppliedATraversal_[S, T, C, D] = AppliedATraversal_(value, optic.andThen(other))

  /** compose this [[ATraversal_]] with an [[ALens_]], having this [[ATraversal_]] applied first */
  final def andThen[C, D](other: ALens_[A, B, C, D]): AppliedATraversal_[S, T, C, D] = AppliedATraversal_(value, optic.andThen(other))

  /** compose this [[ATraversal_]] with a [[Prism_]], having this [[ATraversal_]] applied first */
  final def andThen[C, D](other: Prism_[A, B, C, D]): AppliedATraversal_[S, T, C, D] = AppliedATraversal_(value, optic.andThen(other))

  /** compose this [[ATraversal_]] with an [[APrism_]], having this [[ATraversal_]] applied first */
  final def andThen[C, D](other: APrism_[A, B, C, D]): AppliedATraversal_[S, T, C, D] = AppliedATraversal_(value, optic.andThen(other))

  /** compose this [[ATraversal_]] with an [[AffineTraversal_]], having this [[ATraversal_]] applied first */
  final def andThen[C, D](other: AffineTraversal_[A, B, C, D]): AppliedATraversal_[S, T, C, D] = AppliedATraversal_(value, optic.andThen(other))

  /** compose this [[ATraversal_]] with an [[AnAffineTraversal_]], having this [[ATraversal_]] applied first */
  final def andThen[C, D](other: AnAffineTraversal_[A, B, C, D]): AppliedATraversal_[S, T, C, D] = AppliedATraversal_(value, optic.andThen(other))

  /** compose this [[ATraversal_]] with a [[Traversal_]], having this [[ATraversal_]] applied first */
  final def andThen[C, D](other: Traversal_[A, B, C, D]): AppliedATraversal_[S, T, C, D] = AppliedATraversal_(value, optic.andThen(other))

  /** compose this [[ATraversal_]] with an [[ATraversal_]], having this [[ATraversal_]] applied first */
  final def andThen[C, D](other: ATraversal_[A, B, C, D]): AppliedATraversal_[S, T, C, D] = AppliedATraversal_(value, optic.andThen(other))

  /** compose this [[ATraversal_]] with a [[Setter_]], having this [[ATraversal_]] applied first */
  final def andThen[C, D](other: Setter_[A, B, C, D]): AppliedSetter_[S, T, C, D] = AppliedSetter_(value, optic.andThen(other))

  /** compose this [[ATraversal_]] with a [[Getter_]], having this [[ATraversal_]] applied first */
  final def andThen[C, D](other: Getter_[A, B, C, D]): AppliedFold_[S, T, C, D] = AppliedFold_(value, optic.andThen(other))

  /** compose this [[ATraversal_]] with a [[Fold_]], having this [[ATraversal_]] applied first */
  final def andThen[C, D](other: Fold_[A, B, C, D]): AppliedFold_[S, T, C, D] = AppliedFold_(value, optic.andThen(other))
}

object AppliedATraversal_ {
  def apply[S, T, A, B](_value: S, aTraversal: ATraversal_[S, T, A, B]): AppliedATraversal_[S, T, A, B] =
    new AppliedATraversal_[S, T, A, B] {
      override val value: S = _value
      override val optic: ATraversal_[S, T, A, B] = aTraversal
    }
}

object AppliedATraversal {
  def apply[S, A](_value: S, aTraversal: ATraversal[S, A]): AppliedATraversal[S, A] =
    new AppliedATraversal_[S, S, A, A] {
      override val value: S = _value
      override val optic: ATraversal[S, A] = aTraversal
    }
}
