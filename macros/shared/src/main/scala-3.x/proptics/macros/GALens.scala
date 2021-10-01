package proptics.macros

import proptics.{ALens, Lens}

class GALens[S] {
  /** generate a monomorphic [[ALens]] from a getter */
  inline def apply[A](inline field: S => A): ALens[S, A] = ${ GALensImpl('field) }
}

object GALens {
  def apply[A] = new GALens[A]
}
