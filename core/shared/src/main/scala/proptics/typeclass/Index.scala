package proptics.typeclass

import scala.annotation.implicitNotFound

import cats.syntax.either._

import proptics.AffineTraversal

/** The [[Index]] typeclass provides an [[AffineTraversal]] that can be used to when you either cannot or do not want to add new elements or delete existing ones
  *
  * @tparam S
  *   source of an [[AffineTraversal]]
  * @tparam I
  *   index an [[AffineTraversal]]
  * @tparam A
  *   target of an [[AffineTraversal]]
  */
@implicitNotFound("Could not find an instance of Index[${S}, ${I}, ${A}]")
trait Index[S, I, A] extends Serializable {
  def ix(i: I): AffineTraversal[S, A]
}

object Index {
  /** summon an instance of [[Index]] */
  @inline def apply[S, I, A](implicit ev: Index[S, I, A]): Index[S, I, A] = ev

  /** create an [[Index]], using an index to a viewOrModify function and a setter function */
  final def index[S, I, A](viewOrModify: I => S => Either[S, A])(set: I => S => A => S): Index[S, I, A] = new Index[S, I, A] {
    override def ix(i: I): AffineTraversal[S, A] = AffineTraversal[S, A](viewOrModify(i))(set(i))
  }

  /** create an [[Index]], using an index to a preview function and a setter function */
  def fromPreview[S, I, A](preview: I => S => Option[A])(set: I => S => A => S): Index[S, I, A] =
    Index.index[S, I, A]((i: I) => (s: S) => preview(i)(s).fold(s.asLeft[A])(_.asRight[S]))(set(_))

  /** create an [[Index]], using an index to a partial function and a setter function */
  def fromPartial[S, I, A](preview: I => PartialFunction[S, A])(set: I => S => A => S): Index[S, I, A] =
    fromPreview[S, I, A](i => preview(i).lift)(set(_))
}
