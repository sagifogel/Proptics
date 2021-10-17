package proptics.internal

private[proptics] trait Review0[S, T, A, B] {
  /** view the modified source of a Review */
  def review(b: B): T
}
