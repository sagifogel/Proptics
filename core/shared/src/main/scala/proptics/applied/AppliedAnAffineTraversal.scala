package proptics.applied

import proptics.applied.internal.AppliedAffineTraversal0
import proptics.{AnAffineTraversal, AnAffineTraversal_, AppliedAnAffineTraversal}

trait AppliedAnAffineTraversal_[S, T, A, B] extends AppliedAffineTraversal0[S, T, A, B] {
  val value: S
  val optic: AnAffineTraversal_[S, T, A, B]
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
