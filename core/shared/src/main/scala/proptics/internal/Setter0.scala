package proptics.internal
import scala.Function.const

private[proptics] trait Setter0[S, T, A, B] {
  /** set the modified foci of a Setter */
  final def set(b: B): S => T = over(const(b))

  /** modify the foci type of a Setter using a function, resulting in a change of type to the full structure */
  def over(f: A => B): S => T
}
