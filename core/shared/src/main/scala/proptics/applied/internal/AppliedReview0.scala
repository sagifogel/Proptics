package proptics.applied.internal

import proptics.internal.Review0

private[proptics] trait AppliedReview0[T, B] extends Serializable {
  val optic: Review0[T, B]

  def review(b: B): T = optic.review(b)
}
