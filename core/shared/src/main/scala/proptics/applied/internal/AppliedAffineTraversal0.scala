package proptics.applied.internal

import proptics.applied.AppliedFoldCompat0
import proptics.internal.AffineTraversal0

private[proptics] trait AppliedAffineTraversal0[S, T, A, B] extends AppliedTraversal0[S, T, A, B] with AppliedFoldCompat0[S, A] with AppliedGetter0[S, A] {
  val value: S
  val optic: AffineTraversal0[S, T, A, B]

  /** view the focus of a Traversal or return the modified source of an Traversal */
  def viewOrModify: Either[T, A] = optic.viewOrModify(value)

  /** set the focus of a Traversal conditionally if it is not None */
  final def setOption(b: B): Option[T] = optic.setOption(b)(value)

  /** modify the focus of a Traversal using a function conditionally if it is not None, resulting in a change of type to the full structure */
  final def overOption(f: A => B): Option[T] = optic.overOption(f)(value)

  /** find the first focus of a Getter that satisfies a predicate, if there is any */
  final override def find(f: A => Boolean): Option[A] = optic.find(f)(value)
}
