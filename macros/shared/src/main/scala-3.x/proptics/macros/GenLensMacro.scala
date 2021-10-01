package proptics.macros

import scala.quoted.*

import proptics.{ALens, Iso, Lens, Lens_}

private[macros] class GenLensMacro(val quotes: Quotes) extends Reflection {
  import context.reflect.*
  implicit val context: Quotes = quotes

  def genLens_impl[S: Type, A: Type](field: Expr[S => A]): Expr[Lens[S, A]] =
    genLens[Lens[S, A], S, A](field)

  def genALens_impl[S: Type, A: Type](field: Expr[S => A]): Expr[ALens[S, A]] =
    genLens[ALens[S, A], S, A](field)

  def mkLens[LensType: Type](selectField: SelectField): Term = {
    val simplifiedLens = TypeRepr.of[LensType].simplified

    simplifiedLens.asType match {
      case '[Lens[f, t]] => generateLens(selectField)
      case '[ALens[f, t]] => generateALens(selectField)
    }
  }

  def generateLens(selectField: SelectField): Term =
    import selectField.{caseClassType, fieldName, fieldType, genericArguments}

    (caseClassType.asType, fieldType.asType) match {
      case ('[s], '[a]) =>
        '{
          Lens.apply[s, a]((from: s) => ${ generateGetter('{ from }.asTerm, fieldName).asExprOf[a] })((from: s) =>
            (to: a) => ${ generateSetter('{ from }.asTerm, '{ to }.asTerm, fieldName, genericArguments).asExprOf[s] })
        }.asTerm
    }

  def generateALens(selectField: SelectField): Term =
    import selectField.{caseClassType, fieldName, fieldType, genericArguments}

    (caseClassType.asType, fieldType.asType) match {
      case ('[s], '[a]) =>
        '{
          ALens.apply[s, a]((from: s) => ${ generateGetter('{ from }.asTerm, fieldName).asExprOf[a] })((from: s) =>
            (to: a) => ${ generateSetter('{ from }.asTerm, '{ to }.asTerm, fieldName, genericArguments).asExprOf[s] })
        }.asTerm
    }

  def composeLenses[Lens: Type, S: Type, A: Type](selectFields: List[SelectField]): Either[String, Term] =
    selectFields.foldLeft(zero[S])((acc, selectField) => acc.flatMap(composeOptics(_, mkLens[Lens](selectField))))

  def genLens[Lens: Type, S: Type, A: Type](field: Expr[S => A]): Expr[Lens] = {
    val composed =
      for {
        selectFields <- generateSelectFields(field.asTerm)
        composedLens <- composeLenses[Lens, S, A](selectFields)
      } yield composedLens

    composed.fold(reportErrorAndAdjustType[Lens, S, A].apply, _.asExprOf[Lens])
  }

  def reportErrorAndAdjustType[Lens: Type, S: Type, A: Type](msg: String): Expr[Lens] = {
    report.error(msg)
    '{ null.asInstanceOf[Lens] }.asExprOf[Lens]
  }
}
