package proptics.macros

import scala.quoted.{Expr, Quotes, Type, quotes}

import proptics.{ALens, Lens}

object GALensImpl {
  def apply[S: Type, A: Type](field: Expr[S => A])(using Quotes): Expr[ALens[S, A]] =
    new GenLensMacro(quotes).genALens_impl[S, A](field)
}
