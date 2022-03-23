package proptics.typeclass

import scala.annotation.implicitNotFound

import proptics.Prism

/** The [[Prefixed]] typeclass provides a [[Prism]] for a way to prepend or remove elements on the left side of a structure.
  *
  * @tparam S
  *   the source of a [[Prism]]
  * @tparam T
  *   the focus of a [[Prism]]
  */
@implicitNotFound("Could not find an instance of Prefixed[${S}, ${T}]")
trait Prefixed[S, T] extends Serializable {
  def prefix(s: S): Prism[S, T]
}

object Prefixed {
  /** summon an instance of [[Prefixed]] */
  @inline def apply[S, T](implicit ev: Prefixed[S, T]): Prefixed[S, T] = ev
}
