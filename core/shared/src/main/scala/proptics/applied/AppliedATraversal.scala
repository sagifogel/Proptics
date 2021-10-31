package proptics.applied

import proptics.applied.internal.AppliedTraversal1
import proptics.{ATraversal, ATraversal_, AppliedATraversal}

private[proptics] trait AppliedATraversal_[S, T, A, B] extends AppliedTraversal1[S, T, A, B] {
  val value: S
  val optic: ATraversal_[S, T, A, B]
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
