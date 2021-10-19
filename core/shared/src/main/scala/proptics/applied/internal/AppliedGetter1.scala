package proptics.applied.internal

import cats.data.State

import proptics.internal.Getter1

private[proptics] trait AppliedGetter1[S, A] extends AppliedGetter0[S, A] {
  val value: S
  val optic: Getter1[S, A]

  /** synonym to fold */
  def view: A = optic.view(value)

  /** test whether a predicate holds for the focus of a Getter */
  final override def exists(f: A => Boolean): Boolean = optic.exists(f)(value)

  /** find the first focus of a Getter that satisfies a predicate, if there is any */
  final def find(f: A => Boolean): Option[A] = optic.find(f)(value)

  /** view the focus of a Getter in the state of a monad */
  final def use(implicit ev: State[S, A]): State[S, A] = optic.use
}
