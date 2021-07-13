package proptics.macros

import proptics.APrism

object GAPrism {
  /** generate an [[APrism]] for a sum type `S` and a subtype `A` */
  def apply[S, A <: S]: APrism[S, A] = macro GenPrismMacro.genAPrism_impl[S, A]
}
