package proptics.typeclass

import scala.annotation.implicitNotFound

import proptics.Lens

/** The [[Field4]] typeclass provides a [[Lens]] for a way to get the fourth element of structure.
  *
  * @tparam S the source of a [[Lens]]
  * @tparam A the focus of a [[Lens]]
  */
@implicitNotFound("Could not find an instance of Field4[${S}, ${A}]")
trait Field4[S, A] extends Serializable {
  def fourth: Lens[S, A]
}

object Field4 {
  /** summon an instance of [[Field4]] */
  @inline def apply[S, A](implicit ev: Field4[S, A]): Field4[S, A] = ev
}
