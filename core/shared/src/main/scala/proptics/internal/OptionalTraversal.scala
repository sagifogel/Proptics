package proptics.internal

import scala.Function.const

import proptics.FoldCompat0

trait OptionalTraversal[S, T, A, B] extends Traversal0[S, T, A, B] with FoldCompat0[S, A] with Getter0[S, A] {
  /** view the focus of a Traversal or return the modified source of an Traversal */
  def viewOrModify(s: S): Either[T, A]

  /** set the focus of a Traversal conditionally if it is not None */
  final def setOption(b: B): S => Option[T] = overOption(const(b))

  /** modify the focus of a Traversal using a function conditionally if it is not None, resulting in a change of type to the full structure */
  final def overOption(f: A => B): S => Option[T] = s => preview(s).map(a => set(f(a))(s))

  /** find the first focus of a Getter that satisfies a predicate, if there is any */
  final override def find(f: A => Boolean): S => Option[A] = viewOption(_).filter(f)
}
