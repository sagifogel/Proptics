package proptics.typeclass

import scala.annotation.implicitNotFound

import proptics.Prism

/** The [[Empty]] typeclass provides a [[Prism]] that can tell whether a structure is empty.
  *
  * @tparam S the source of a [[Prism]]
  */
@implicitNotFound("Could not find an instance of Empty[${S}]")
trait Empty[S] extends Serializable {
  def empty: Prism[S, Unit]
}

object Empty {
  /** summon an instance of [[Empty]] */
  @inline def apply[S](implicit ev: Empty[S]): Empty[S] = ev
}
