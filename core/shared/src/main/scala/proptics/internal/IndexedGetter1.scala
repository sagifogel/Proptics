package proptics.internal

import scala.Function.const

import cats.data.State
import cats.syntax.option._

trait IndexedGetter1[I, S, A] extends IndexedGetter0[I, S, A] {
  /** view the focus of a Getter */
  def view(s: S): (A, I)

  protected def viewOption(s: S): Option[(A, I)] = view(s).some

  /** test whether a predicate holds for the focus of a Getter */
  final override def exists(f: ((A, I)) => Boolean): S => Boolean =
    find(f)(_).fold(false)(const(true))

  /** view the focus of a Getter in the state of a monad */
  final def use(implicit ev: State[S, A]): State[S, (A, I)] = ev.inspect(view)
}
