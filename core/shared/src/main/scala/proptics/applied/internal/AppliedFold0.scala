package proptics.applied.internal

import proptics.internal.Fold0

private[proptics] trait AppliedFold0[S, A] extends AppliedGetter0[S, A] with Serializable {
  val value: S
  val optic: Fold0[S, A]

  /** check if the Fold does not contain a focus */
  final def isEmpty: Boolean = optic.isEmpty(value)

  /** check if the Fold contains a focus */
  final def nonEmpty: Boolean = optic.nonEmpty(value)

  /** view the first focus of a Fold, if there is any */
  final def preview: Option[A] = optic.preview(value)

  /** test whether there is no focus or a predicate holds for the focus of a Fold */
  def forall(f: A => Boolean): Boolean = optic.preview(value).forall(f)
}
