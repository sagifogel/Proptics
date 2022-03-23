package proptics.typeclass

import scala.annotation.implicitNotFound

import proptics.Prism

/** The [[Suffixed]] typeclass provides a [[Prism]] for a way to append or truncate elements on the right side of a structure.
  *
  * @tparam S
  *   the source of a [[Prism]]
  * @tparam T
  *   the focus of a [[Prism]]
  */
@implicitNotFound("Could not find an instance of Suffixed[${S}, ${T}]")
trait Suffixed[S, T] extends Serializable {
  def suffix(s: S): Prism[S, T]
}

object Suffixed {
  /** summon an instance of [[Suffixed]] */
  @inline def apply[S, T](implicit ev: Suffixed[S, T]): Suffixed[S, T] = ev
}
