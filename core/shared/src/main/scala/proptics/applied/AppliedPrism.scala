package proptics.applied

import proptics.applied.internal.{AppliedAffineTraversal0, AppliedReview0}
import proptics.{AppliedPrism, Prism, Prism_}

trait AppliedPrism_[S, T, A, B] extends AppliedAffineTraversal0[S, T, A, B] with AppliedReview0[T, B] {
  val value: S
  val optic: Prism_[S, T, A, B]
}

object AppliedPrism_ {
  def apply[S, T, A, B](_value: S, prism: Prism_[S, T, A, B]): AppliedPrism_[S, T, A, B] =
    new AppliedPrism_[S, T, A, B] {
      override val value: S = _value
      override val optic: Prism_[S, T, A, B] = prism
    }
}

object AppliedPrism {
  def apply[S, A](_value: S, prism: Prism[S, A]): AppliedPrism[S, A] =
    new AppliedPrism[S, A] {
      override val value: S = _value
      override val optic: Prism[S, A] = prism
    }
}
