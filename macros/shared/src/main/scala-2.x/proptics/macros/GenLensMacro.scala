package proptics.macros

import scala.annotation.tailrec
import scala.reflect.macros.blackbox

import cats.instances.string._
import cats.syntax.eq._

import proptics.{ALens, Lens}

private[macros] class GenLensMacro(val c: blackbox.Context) {
  import c.universe._

  object FieldsMeta {
    def unapply(tree: Tree): Option[Seq[(TermName, Type)]] = {
      @tailrec
      def go(tree: Tree, fields: Option[List[(TermName, Type)]]): Option[List[(TermName, Type)]] =
        tree match {
          case Select(tail: Ident, field: TermName) =>
            fields.map((field, tail.tpe.widen) :: _)
          case Select(tail, field: TermName) =>
            go(tail, fields.map((field, tail.tpe.widen) :: _))
          case _ => None
        }

      go(tree, Some(List.empty[(TermName, Type)]))
    }
  }

  def genLens_impl[S: c.WeakTypeTag, A: c.WeakTypeTag](field: c.Expr[S => A]): c.Expr[Lens[S, A]] =
    genLens[Lens[S, A], GLens[S], S, A](field)

  def genALens_impl[S: c.WeakTypeTag, A: c.WeakTypeTag](field: c.Expr[S => A]): c.Expr[ALens[S, A]] =
    genLens[ALens[S, A], GALens[S], S, A](field)

  def genLens[Lens: c.WeakTypeTag, GenLens: c.WeakTypeTag, S: c.WeakTypeTag, A: c.WeakTypeTag](field: c.Expr[S => A]): c.Expr[Lens] =
    field match {
      case Expr(Function(_, Select(_: Ident, name))) =>
        val tree = q"${name.decodedName.toString}"
        mkLens[Lens, S, A](c.Expr[String](tree))

      case Expr(Function(_, FieldsMeta(fields))) =>
        val genLensName = symbolOf[GenLens].name.toTermName
        val composedLensesTree =
          fields
            .map { case (field, tpe) => q"_root_.proptics.macros.$genLensName[$tpe](_.$field)" }
            .reduce((a, b) => q"$a.andThen($b)")
        c.Expr[Lens](composedLensesTree)

      case _ => c.abort(c.enclosingPosition, s"could not focus on field ${show(field.tree)}")
    }

  def mkLens[Lens: c.WeakTypeTag, S: c.WeakTypeTag, A: c.WeakTypeTag](fieldExpression: c.Expr[String]): c.Expr[Lens] = {
    val typeOfS = weakTypeOf[S]
    val lensName = symbolOf[Lens].name.toTermName
    val untypedTree = c.untypecheck(fieldExpression.tree.duplicate)
    val fieldName = c.eval(c.Expr[String](untypedTree))
    val field =
      typeOfS.decls
        .collectFirst {
          case m: MethodSymbol if m.isPrimaryConstructor =>
            m.paramLists.head
              .find(_.name.decodedName.toString === fieldName)
        }
        .flatten
        .getOrElse(c.abort(c.enclosingPosition, s"Cannot find field $fieldName in $typeOfS"))
    val fieldGetter =
      typeOfS.decls
        .collectFirst { case m: MethodSymbol if m.isCaseAccessor && m.name.decodedName.toString === fieldName => m.getter }
        .getOrElse(c.abort(c.enclosingPosition, s"Cannot find method $fieldName in $typeOfS"))
    val lensTree = q"_root_.proptics.$lensName[$typeOfS, ${weakTypeOf[A]}](_.$fieldGetter)(s => a => s.copy($field = a))"

    c.Expr[Lens](lensTree)
  }
}
