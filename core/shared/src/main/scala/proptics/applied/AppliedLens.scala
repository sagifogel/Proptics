package proptics.applied

import cats.Alternative
import cats.arrow.Strong

import proptics._
import proptics.applied.internal.{AppliedGetter1, AppliedTraversal0}
import proptics.data.Disj
import proptics.profunctor.Star

trait AppliedLens_[S, T, A, B] extends AppliedTraversal0[S, T, A, B] with AppliedGetter1[S, A] {
  val value: S
  val optic: Lens_[S, T, A, B]

  /** compose this [[Lens_]] with a function lifted to a [[Getter_]], having this [[Lens_]] applied first */
  final def focus[C, D](f: A => C): AppliedGetter_[S, T, C, D] = AppliedGetter_(value, optic.focus(f))

  /** try to map a function over this [[Lens_]], failing if the [[Lens_]] has no focus. */
  def failover[F[_]](f: A => B)(implicit ev0: Strong[Star[(Disj[Boolean], *), *, *]], ev1: Alternative[F]): F[T] = optic.failover(f)(value)

  /** zip two sources of a [[Lens_]] together provided a binary operation which modify the focus type of a [[Lens_]] */
  final def zipWith[F[_]](s: S)(f: (A, A) => B): T = optic.zipWith(value, s)(f)

  /** compose this [[Lens_]] with an [[Iso_]], having this [[Lens_]] applied first */
  final def andThen[C, D](other: Iso_[A, B, C, D]): AppliedLens_[S, T, C, D] = AppliedLens_[S, T, C, D](value, optic.andThen(other))

  /** compose this [[Lens_]] with an [[AnIso_]], having this [[Lens_]] applied first */
  final def andThen[C, D](other: AnIso_[A, B, C, D]): AppliedLens_[S, T, C, D] = AppliedLens_[S, T, C, D](value, optic.andThen(other))

  /** compose this [[Lens_]] with a [[Lens_]], having this [[Lens_]] applied first */
  final def andThen[C, D](other: Lens_[A, B, C, D]): AppliedLens_[S, T, C, D] = AppliedLens_[S, T, C, D](value, optic.andThen(other))

  /** compose this [[Lens_]] with an [[ALens_]], having this [[Lens_]] applied first */
  final def andThen[C, D](other: ALens_[A, B, C, D]): AppliedLens_[S, T, C, D] = AppliedLens_[S, T, C, D](value, optic.andThen(other))

  /** compose this [[Lens_]] with a [[Prism_]], having this [[Lens_]] applied first */
  final def andThen[C, D](other: Prism_[A, B, C, D]): AppliedAffineTraversal_[S, T, C, D] = AppliedAffineTraversal_[S, T, C, D](value, optic.andThen(other))

  /** compose this [[Lens_]] with an [[APrism_]], having this [[Lens_]] applied first */
  final def andThen[C, D](other: APrism_[A, B, C, D]): AppliedAffineTraversal_[S, T, C, D] = AppliedAffineTraversal_[S, T, C, D](value, optic.andThen(other))

  /** compose this [[Lens_]] with a [[AffineTraversal_]], having this [[Lens_]] applied first */
  final def andThen[C, D](other: AffineTraversal_[A, B, C, D]): AppliedAffineTraversal_[S, T, C, D] = AppliedAffineTraversal_[S, T, C, D](value, optic.andThen(other))

  /** compose this [[Lens_]] with an [[AnAffineTraversal_]], having this [[Lens_]] applied first */
  final def andThen[C, D](other: AnAffineTraversal_[A, B, C, D]): AppliedAnAffineTraversal_[S, T, C, D] = AppliedAnAffineTraversal_[S, T, C, D](value, optic.andThen(other))

  /** compose this [[Lens_]] with a [[Traversal_]], having this [[Lens_]] applied first */
  final def andThen[C, D](other: Traversal_[A, B, C, D]): AppliedTraversal_[S, T, C, D] = AppliedTraversal_[S, T, C, D](value, optic.andThen(other))

  /** compose this [[Lens_]] with an [[ATraversal_]], having this [[Lens_]] applied first */
  final def andThen[C, D](other: ATraversal_[A, B, C, D]): AppliedATraversal_[S, T, C, D] = AppliedATraversal_[S, T, C, D](value, optic.andThen(other))

  /** compose this [[Lens_]] with a [[Setter_]], having this [[Lens_]] applied first */
  final def andThen[C, D](other: Setter_[A, B, C, D]): AppliedSetter_[S, T, C, D] = AppliedSetter_[S, T, C, D](value, optic.andThen(other))

  /** compose this [[Lens_]] with a [[Getter_]], having this [[Lens_]] applied first */
  final def andThen[C, D](other: Getter_[A, B, C, D]): AppliedGetter_[S, T, C, D] = AppliedGetter_[S, T, C, D](value, optic.andThen(other))

  /** compose this [[Lens_]] with a [[Fold_]], having this [[Lens_]] applied first */
  final def andThen[C, D](other: Fold_[A, B, C, D]): AppliedFold_[S, T, C, D] = AppliedFold_[S, T, C, D](value, optic.andThen(other))
}

object AppliedLens_ {
  def apply[S, T, A, B](_value: S, lens: Lens_[S, T, A, B]): AppliedLens_[S, T, A, B] =
    new AppliedLens_[S, T, A, B] {
      override val value: S = _value
      override val optic: Lens_[S, T, A, B] = lens
    }
}

object AppliedLens {
  def apply[S, A](_value: S, lens: Lens[S, A]): AppliedLens[S, A] =
    new AppliedLens_[S, S, A, A] {
      override val value: S = _value
      override val optic: Lens[S, A] = lens
    }
}
