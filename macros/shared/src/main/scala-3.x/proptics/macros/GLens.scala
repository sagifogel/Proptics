package proptics.macros

import proptics.{ALens, Lens}

class GLens[S] {
  /** generate a monomorphic [[Lens]] from a getter */
  inline def apply[A](inline field: S => A): Lens[S, A] = ${ GLensImpl('field) }
}

object GLens {
  def apply[A] = new GLens[A]
}
