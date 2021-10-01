package proptics.macros

import scala.quoted.{Expr, Quotes, Type, quotes}

import proptics.APrism

object GenAPrismImpl {
  def apply[S: Type, A: Type](using Quotes): Expr[APrism[S, A]] =
    new GenPrismMacro(quotes).genAPrism_impl[S, A]
}
