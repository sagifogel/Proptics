package proptics.applied.internal

import proptics.internal.Fold0

private[proptics] trait AppliedFold0[S, A] extends Serializable {
  val value: S
  val optic: Fold0[S, A]

  /** view the first focus of a Fold, if there is any */
  final def preview: Option[A] = optic.preview(value)

  /** test whether a predicate holds for the focus of a Getter */
  final def exists(f: A => Boolean): Boolean = optic.exists(f)(value)

  /** test whether there is no focus or a predicate holds for the focus of a Fold */
  def forall(f: A => Boolean): Boolean = optic.preview(value).forall(f)
}
