package proptics.internal

import scala.Function.const

trait IndexedSetter0[I, S, T, A, B] extends Serializable {
  /** set the modified focus of an IndexedSetter */
  final def set(b: B): S => T = over(const(b))

  /** modify the focus type of an IndexedSetter using a function, resulting in a change of type to the full structure */
  def over(f: ((A, I)) => B): S => T
}
