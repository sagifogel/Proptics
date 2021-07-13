package proptics.macros

import proptics.Prism

object GPrism {
  /** generate a [[Prism]] for a sum type `S` and a subtype `A` */
  def apply[S, A <: S]: Prism[S, A] = macro GenPrismMacro.genPrism_impl[S, A]
}
