package proptics.typeclass

import scala.annotation.implicitNotFound

import proptics.Lens

/** The [[Field1]] typeclass provides a [[Lens]] for a way to get the first element of structure.
  *
  * @tparam S the source of a [[Lens]]
  * @tparam A the focus of a [[Lens]]
  */
@implicitNotFound("Could not find an instance of Field1[${S}, ${A}]")
trait Field1[S, A] extends Serializable {
  def first: Lens[S, A]
}

object Field1 {
  /** summon an instance of [[Field1]] */
  @inline def apply[S, A](implicit ev: Field1[S, A]): Field1[S, A] = ev
}
