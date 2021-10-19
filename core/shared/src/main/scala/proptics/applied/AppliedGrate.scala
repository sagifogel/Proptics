package proptics.applied

import proptics.{AppliedGrate, Grate, Grate_}

trait AppliedGrate_[S, T, A, B] extends Serializable {
  val value: S
  val optic: Grate_[S, T, A, B]
}

object AppliedGrate_ {
  def apply[S, T, A, B](_value: S, grate: Grate_[S, T, A, B]): AppliedGrate_[S, T, A, B] =
    new AppliedGrate_[S, T, A, B] {
      override val value: S = _value
      override val optic: Grate_[S, T, A, B] = grate
    }
}

object AppliedGrate {
  def apply[S, T, A, B](_value: S, grate: Grate[S, A]): AppliedGrate[S, A] =
    new AppliedGrate[S, A] {
      override val value: S = _value
      override val optic: Grate[S, A] = grate
    }
}
