package proptics.macros

import proptics.Lens

class GLens[S] {
  /** generate a monomorphic [[Lens]] from a getter */
  def apply[A](field: S => A): Lens[S, A] = macro Macros.genLens_impl[S, A]
}

object GLens {
  def apply[A] = new GLens[A]
}
