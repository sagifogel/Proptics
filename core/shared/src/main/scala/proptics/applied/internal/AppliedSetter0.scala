package proptics.applied.internal

import proptics.internal.Setter0

private[proptics] trait AppliedSetter0[S, T, A, B] extends Serializable {
  val value: S
  val optic: Setter0[S, T, A, B]

  /** set the modified foci of a Setter */
  final def set(b: B): T = optic.set(b)(value)

  /** modify the foci type of a Setter using a function, resulting in a change of type to the full structure */
  def over(f: A => B): T = optic.over(f)(value)
}
