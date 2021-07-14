package proptics.macros

import scala.reflect.macros.blackbox

import proptics.{APrism, Prism}

private[macros] class GenPrismMacro(val c: blackbox.Context) {
  import c.universe._

  def genPrism_impl[S: c.WeakTypeTag, A: c.WeakTypeTag]: c.Expr[Prism[S, A]] = genPrism[Prism[S, A], S, A]

  def genAPrism_impl[S: c.WeakTypeTag, A: c.WeakTypeTag]: c.Expr[APrism[S, A]] = genPrism[APrism[S, A], S, A]

  def genPrism[Prism: c.WeakTypeTag, S: c.WeakTypeTag, A: c.WeakTypeTag]: c.Expr[Prism] = {
    val typeOfS = weakTypeOf[S]
    val typeOfA = weakTypeOf[A]
    val prismName = symbolOf[Prism].name.toTermName
    val prismTree =
      q"""  
          _root_.proptics.$prismName.fromPartial[$typeOfS, $typeOfA] { case s if s.isInstanceOf[$typeOfA] => s.asInstanceOf[$typeOfA] }(_.asInstanceOf[$typeOfS])
       """

    c.Expr[Prism](prismTree)
  }
}
