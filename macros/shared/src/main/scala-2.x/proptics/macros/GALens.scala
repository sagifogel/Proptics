package proptics.macros

import proptics.ALens

class GALens[S] {
  /** generate a monomorphic [[ALens]] from a getter */
  def apply[A](field: S => A): ALens[S, A] = macro Macros.genALens_impl[S, A]
}

object GALens {
  def apply[S] = new GALens[S]
}
