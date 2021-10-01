package proptics.macros

import scala.quoted.{Quotes, _}

import proptics.{APrism, APrism_, Prism, Prism_}

private[macros] class GenPrismMacro(val quotes: Quotes) {
  import context.reflect._
  implicit val context: Quotes = quotes

  def genPrism_impl[S: Type, A: Type]: Expr[Prism[S, A]] =
    '{ Prism.fromPartial[S, A] { case s if s.isInstanceOf[A] => s.asInstanceOf[A] }(_.asInstanceOf[S]) }

  def genAPrism_impl[S: Type, A: Type]: Expr[APrism[S, A]] =
    '{ APrism.fromPartial[S, A] { case s if s.isInstanceOf[A] => s.asInstanceOf[A] }(_.asInstanceOf[S]) }
}
