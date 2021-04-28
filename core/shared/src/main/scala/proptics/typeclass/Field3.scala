package proptics.typeclass

import scala.annotation.implicitNotFound

import proptics.Lens

/** The [[Field3]] typeclass provides a [[Lens]] for a way to get the third element of structure.
 *
 * @tparam S the source of a [[Lens]]
 * @tparam A the focus of a [[Lens]]
 */
@implicitNotFound("Could not find an instance of Field3[${S}, ${A}]")
trait Field3[S, A] extends Serializable {
  def third: Lens[S, A]
}

object Field3 {
  /** summon an instance of [[Field3]] */
  @inline def apply[S, A](implicit ev: Field3[S, A]): Field3[S, A] = ev
}
