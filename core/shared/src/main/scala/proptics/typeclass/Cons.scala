package proptics.typeclass

import scala.annotation.implicitNotFound

import proptics.instances.field1._
import proptics.instances.field2._
import proptics.std.tuple._
import proptics.{AffineTraversal, Prism}

/** The [[Cons]] typeclass provides a [[Prism]] for a way to attach or detach elements on the left side of a structure.
  *
  * @tparam S
  *   the source of a [[Prism]]
  * @tparam A
  *   the head part of the focus `(A, S)` of a [[Prism]]
  */
@implicitNotFound("Could not find an instance of Cons[${S}, ${A}]")
trait Cons[S, A] extends Serializable {
  def cons: Prism[S, (A, S)]

  def headOption: AffineTraversal[S, A] = cons andThen _1[A, S]

  def tailOption: AffineTraversal[S, S] = cons andThen _2[A, S]
}

object Cons {
  /** summon an instance of [[Cons]] */
  @inline def apply[S, A](implicit ev: Cons[S, A]): Cons[S, A] = ev
}
