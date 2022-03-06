package proptics.internal

private[proptics] trait Review0[T, B] {
  /** view the modified source of a Review */
  def review(b: B): T
}
