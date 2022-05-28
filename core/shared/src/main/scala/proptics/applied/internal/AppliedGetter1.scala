package proptics.applied.internal

import cats.data.State

import proptics.internal.Getter1

private[proptics] trait AppliedGetter1[S, A] extends AppliedGetter0[S, A] {
  val value: S
  val optic: Getter1[S, A]

  /** synonym for fold */
  def view: A = optic.view(value)

  /** view the focus of a Getter in the state of a monad */
  final def use(implicit ev: State[S, A]): State[S, A] = optic.use(ev)
}
