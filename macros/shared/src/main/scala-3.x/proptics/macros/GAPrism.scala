package proptics.macros

import proptics.{APrism, Prism}

object GAPrism {
  /** generate an [[APrism]] for a sum type `S` and a subtype `A` */
  inline def apply[S, A <: S]: APrism[S, A] = ${ GenAPrismImpl.apply }
}
