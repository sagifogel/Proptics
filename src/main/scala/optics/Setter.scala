package optics

import scala.Function.const

/**
 * A [[Setter]] is an [[Optic]] with a fixed type of a [[Function1]] as the type constructor
 *
 * @tparam S the source of a [[Setter]]
 * @tparam T the modified source of a [[Setter]]
 * @tparam A the target of a [[Setter]]
 * @tparam B the modified target of a [[Setter]]
 */
abstract class Setter[S, T, A, B] extends Optic[* => *, S, T, A, B] { self =>
  def over(f: A => B): S => T = self(f)

  def set(b: B): S => T = over(const(b))
}

object Setter {
  def apply[S, T, A, B](f: (A => B) => S => T): Setter[S, T, A, B] = new Setter[S, T, A, B] {
    override def apply(pab: A => B): S => T = f(pab)
  }
}
