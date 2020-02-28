package proptics

/**
 * A [[Setter]] is an [[Optic]] with a fixed type of a [[Function1]] as the type constructor
 *
 * @tparam S the source of a [[Setter]]
 * @tparam T the modified source of a [[Setter]]
 * @tparam A the target of a [[Setter]]
 * @tparam B the modified target of a [[Setter]]
 */
abstract class Setter[S, T, A, B] { self =>
   def apply(pab: A => B): S => T
}

object Setter {
  def apply[S, T, A, B](f: (A => B) => S => T): Setter[S, T, A, B] = new Setter[S, T, A, B] {
    override def apply(pab: A => B): S => T = f(pab)
  }
}

object Setter_ {
  def apply[S, A](f: (A => A) => S => S): Setter[S, S, A, A] = Setter[S, S, A, A](f)
}