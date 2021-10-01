package proptics.macros

import scala.quoted.{Expr, Quotes, Type, quotes}

import proptics.{ALens, Lens}

object GLensImpl {
  def apply[S: Type, A: Type](field: Expr[S => A])(using Quotes): Expr[Lens[S, A]] =
    new GenLensMacro(quotes).genLens_impl[S, A](field)
}
