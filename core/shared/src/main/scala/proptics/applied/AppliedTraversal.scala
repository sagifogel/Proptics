package proptics.applied

import proptics.applied.internal._
import proptics.{AppliedTraversal, Traversal, Traversal_}

trait AppliedTraversal_[S, T, A, B] extends AppliedTraversal1[S, T, A, B] {
  val value: S
  val optic: Traversal_[S, T, A, B]
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
