package proptics.typeclass

import scala.annotation.implicitNotFound

import proptics.Lens

/** The [[Field2]] typeclass provides a [[Lens]] for a way to get the second element of structure.
  *
  * @tparam S the source of a [[Lens]]
  * @tparam A the focus of a [[Lens]]
  */
@implicitNotFound("Could not find an instance of Field2[${S}, ${A}]")
trait Field2[S, A] extends Serializable {
  def second: Lens[S, A]
}

object Field2 {
  /** summon an instance of [[Field2]] */
  @inline def apply[S, A](implicit ev: Field2[S, A]): Field2[S, A] = ev
}
