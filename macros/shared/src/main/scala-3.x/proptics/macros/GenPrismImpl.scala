package proptics.macros

import scala.quoted.{Expr, Quotes, Type, quotes}

import proptics.Prism

object GenPrismImpl {
  def apply[S: Type, A: Type](using Quotes): Expr[Prism[S, A]] =
    new GenPrismMacro(quotes).genPrism_impl[S, A]
}
