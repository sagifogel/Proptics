package proptics.macros

import scala.annotation.tailrec
import scala.quoted.*

import cats.instances.string.*
import cats.syntax.eq.*

import proptics.Iso

private[macros] trait Reflection {
  given Quotes = context
  val context: Quotes
  import context.reflect.*

  def zero[S: Type]: Either[String, Term] = Right('{ Iso.id[S] }.asTerm)

  case class SelectField(fieldName: String, caseClassType: TypeRepr, genericArguments: List[TypeRepr], fieldType: TypeRepr)

  object SelectField {
    def apply(fromType: TypeRepr, fieldType: TypeRepr, fieldName: String): SelectField =
      SelectField(fieldName, fromType, getGenericTypeArguments(fromType), fieldType)
  }

  def getClassSymbol(classTypeRepr: TypeRepr): Either[String, Symbol] =
    classTypeRepr.classSymbol
      .toRight(s"could not find concrete type for ${classTypeRepr.show}")

  def unexpectedBehavior(term: Term): Either[String, List[SelectField]] =
    Left(s"undefined behavior for code ${term.show}")

  def couldNotFindConcreteType(typeRepr: TypeRepr): String =
    s"could not find concrete type for ${typeRepr.widen.show}"

  def extractType(typeRepr: TypeRepr, error: => String): Either[String, TypeRepr] =
    typeRepr match {
      case AppliedType(tpe, _) => Right(tpe)
      case _ => Left(error)
    }

  def extractTypeFromSymbol(symbol: Symbol, error: => String): Either[String, TypeRepr] =
    symbol.tree match {
      case ValDef(_, typeTree, _) => Right(typeTree.tpe)
      case _ => Left(error)
    }

  def getFieldType(classTypeRepr: TypeRepr, fieldName: String): Either[String, TypeRepr] = {
    lazy val fieldTypeError: String =
      s"could not find field: $fieldName for type: ${classTypeRepr.show}"

    def getFieldSymbol(fromTypeSymbol: Symbol): Either[String, Symbol] =
      fromTypeSymbol.memberFields
        .find(_.name.trim === fieldName)
        .toRight(fieldTypeError)

    for {
      classSymbol <- getClassSymbol(classTypeRepr)
      fieldSymbol <- getFieldSymbol(classSymbol)
      fieldType <- extractTypeFromSymbol(fieldSymbol, fieldTypeError)
    } yield makeConcreteTypeRepr(classTypeRepr, fieldType)
  }

  private def isCaseClass(term: Term): Boolean =
    term.tpe.classSymbol.exists(_.flags.is(Flags.Case))

  private def getGenericTypeParameters(classTypeRepr: TypeRepr): List[Symbol] =
    classTypeRepr.classSymbol
      .flatMap(_.primaryConstructor.paramSymss.headOption)
      .filter(_.exists(_.isTypeParam))
      .getOrElse(Nil)

  def getGenericTypeArguments(classTypeRepr: TypeRepr): List[TypeRepr] =
    classTypeRepr match {
      case AppliedType(_, argTypeReprs) => argTypeReprs
      case _ => Nil
    }

  def applyType(typeRepr: TypeRepr, argsByName: Map[String, TypeRepr]): TypeRepr =
    typeRepr match {
      case AppliedType(tpe, args) =>
        val appliedTypeRepr = applyType(tpe, argsByName)
        val appliedArgs = args.map(applyType(_, argsByName))
        appliedTypeRepr.appliedTo(appliedArgs)
      case tpe =>
        argsByName.getOrElse(typeRepr.typeSymbol.name, tpe)
    }

  private def makeConcreteTypeRepr(classTypeRepr: TypeRepr, fieldTypeRepr: TypeRepr): TypeRepr = {
    val genericParameters = getGenericTypeParameters(classTypeRepr)
    val genericArguments = getGenericTypeArguments(classTypeRepr)
    val argsByName =
      genericParameters
        .map(_.name)
        .zip(genericArguments)
        .toMap

    applyType(fieldTypeRepr, argsByName)
  }

  def generateSelectFields(term: Term): Either[String, List[SelectField]] =
    term match {
      case InlinedTerm(SelectFields(selectFields)) => selectFields
      case _ => unexpectedBehavior(term)
    }

  object SelectFields {
    private def addField(term: Term, fields: Either[String, List[SelectField]], fieldName: String): Either[String, List[SelectField]] =
      for {
        selectFieldsList <- fields
        field <- selectField(term.tpe.widen, fieldName)
      } yield field :: selectFieldsList

    private def selectField(caseClassTypeRepr: TypeRepr, fieldName: String): Either[String, SelectField] =
      getFieldType(caseClassTypeRepr, fieldName).map(SelectField(caseClassTypeRepr, _, fieldName))

    def unapply(term: Term): Option[Either[String, List[SelectField]]] = {
      @tailrec
      def go(term: Term, fields: Either[String, List[SelectField]]): Either[String, List[SelectField]] =
        term match {
          case Ident(_) => fields
          case Select(tail, fieldName) if isCaseClass(tail) =>
            go(tail, addField(tail, fields, fieldName))
          case Select(tail, _) =>
            Left(s"could not generate Lens, because ${tail.tpe.widen.show} is not a case class")
          case _ => unexpectedBehavior(term)
        }

      Some(go(term, Right(Nil)))
    }
  }

  def generateGetter(caseClassTerm: Term, fieldName: String): Term = Select.unique(caseClassTerm, fieldName)

  def generateSetter(caseClassTerm: Term, fieldTerm: Term, fieldName: String, genericArguments: List[TypeRepr]): Term =
    Select.overloaded(caseClassTerm, "copy", genericArguments, NamedArg(fieldName, fieldTerm) :: Nil)

  def composeOptics(opticTerm1: Term, opticTerm2: Term): Either[String, Term] =
    opticTerm2.tpe.widen match {
      case AppliedType(_, List(_, toTypeRepr)) => Right(Select.overloaded(opticTerm1, "andThen", List(toTypeRepr, toTypeRepr), List(opticTerm2)))
      case _ => Left(s"undefined behavior for composition of ${opticTerm1.tpe.widen.show} andThen ${opticTerm2.tpe.widen.show}")
    }

  object InlinedTerm {
    def unapply(term: Term): Option[Term] = term match {
      case Inlined(_, _, Block(List(DefDef(_, _, _, select @ Some(Select(_, _)))), _)) => select
      case _ => None
    }
  }
}
