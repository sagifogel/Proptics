package proptics.internal

/**
  * @tparam S the source of a [[proptics.Prism_]]
  * @tparam T the modified source of a [[proptics.Prism_]]
  * @tparam A the focus of a [[proptics.Prism_]]
  */
private[proptics] trait PrismFunctions[S, T, A] {
  def viewOrModify(s: S): Either[T, A]
}
