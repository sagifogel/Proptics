package proptics.typeclass

import scala.annotation.implicitNotFound

import proptics.Lens

/** The [[Field5]] typeclass provides a [[Lens]] for a way to get the fifth element of structure.
  *
  * @tparam S the source of a [[Lens]]
  * @tparam A the focus of a [[Lens]]
  */
@implicitNotFound("Could not find an instance of Field5[${S}, ${A}]")
trait Field5[S, A] extends Serializable {
  def fifth: Lens[S, A]
}

object Field5 {
  /** summon an instance of [[Field5]] */
  @inline def apply[S, A](implicit ev: Field5[S, A]): Field5[S, A] = ev
}
