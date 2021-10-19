package proptics.applied

import proptics.applied.internal.AppliedAffineTraversal0
import proptics.{AffineTraversal, AffineTraversal_, AppliedAffineTraversal}

trait AppliedAffineTraversal_[S, T, A, B] extends AppliedAffineTraversal0[S, T, A, B] {
  val value: S
  val optic: AffineTraversal_[S, T, A, B]
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
