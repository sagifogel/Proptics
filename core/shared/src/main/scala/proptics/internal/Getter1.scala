package proptics.internal

import scala.Function.const

import cats.data.State
import cats.syntax.option._

private[proptics] trait Getter1[S, A] extends Getter0[S, A] {
  /** view the focus of a Getter */
  def view(s: S): A

  protected def viewOption(s: S): Option[A] = view(s).some

  /** test whether a predicate holds for the focus of a Getter */
  final override def exists(f: A => Boolean): S => Boolean =
    find(f)(_).fold(false)(const(true))

  /** find the first focus of a Getter that satisfies a predicate, if there is any */
  final def find(f: A => Boolean): S => Option[A] = viewOption(_).filter(f)

  /** view the focus of a Getter in the state of a monad */
  final def use(implicit ev: State[S, A]): State[S, A] = ev.inspect(view)
}
