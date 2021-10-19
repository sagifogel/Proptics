package proptics.applied

import proptics.applied.internal.{AppliedAffineTraversal0, AppliedReview0}
import proptics.{APrism, APrism_, AppliedAPrism}

trait AppliedAPrism_[S, T, A, B] extends AppliedAffineTraversal0[S, T, A, B] with AppliedReview0[T, B] {
  val value: S
  val optic: APrism_[S, T, A, B]
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
