package proptics.applied

import proptics.applied.internal.AppliedSetter0
import proptics.{AppliedSetter, Setter, Setter_}

trait AppliedSetter_[S, T, A, B] extends AppliedSetter0[S, T, A, B] {
  val value: S
  val optic: Setter_[S, T, A, B]
}

object AppliedSetter_ {
  def apply[S, T, A, B](_value: S, setter: Setter_[S, T, A, B]): AppliedSetter_[S, T, A, B] =
    new AppliedSetter_[S, T, A, B] {
      override val value: S = _value
      override val optic: Setter_[S, T, A, B] = setter
    }
}

object AppliedSetter {
  def apply[S, A](_value: S, setter: Setter[S, A]): AppliedSetter[S, A] =
    new AppliedSetter[S, A] {
      override val value: S = _value
      override val optic: Setter[S, A] = setter
    }
}
