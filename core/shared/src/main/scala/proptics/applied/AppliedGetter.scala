package proptics.applied

import proptics.applied.internal.AppliedGetter1
import proptics.{AppliedGetter, Getter, Getter_}

trait AppliedGetter_[S, T, A, B] extends AppliedGetter1[S, A] {
  val value: S
  val optic: Getter_[S, T, A, B]
}

object AppliedGetter_ {
  def apply[S, T, A, B](_value: S, getter: Getter_[S, T, A, B]): AppliedGetter_[S, T, A, B] =
    new AppliedGetter_[S, T, A, B] {
      override val value: S = _value
      override val optic: Getter_[S, T, A, B] = getter
    }
}

object AppliedGetter {
  def apply[S, A](_value: S, getter: Getter[S, A]): AppliedGetter[S, A] =
    new AppliedGetter[S, A] {
      override val value: S = _value
      override val optic: Getter[S, A] = getter
    }
}
