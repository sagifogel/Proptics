package proptics.internal

import scala.Function.const

private[proptics] trait AffineTraversal0[S, T, A, B] extends Traversal0[S, T, A, B] with Fold0[S, A] {
  /** view the focus or return the modified source of an AffineTraversal */
  def viewOrModify(s: S): Either[T, A]

  /** set the focus of an AffineTraversal conditionally if it is not None */
  final def setOption(b: B): S => Option[T] = overOption(const(b))

  /** modify the focus of an AffineTraversal using a function conditionally if it is not None, resulting in a change of type to the full structure */
  final def overOption(f: A => B): S => Option[T] = s => preview(s).map(a => set(f(a))(s))

  /** find the first focus of an AffineTraversal that satisfies a predicate, if there is any */
  final override def find(f: A => Boolean): S => Option[A] = preview(_).filter(f)
}
