package proptics.typeclass

import scala.annotation.implicitNotFound

import proptics.Iso

/** The [[Reverse]] typeclass provides an [[Iso]] for a way to reverse the elements of a structure.
  *
  * @tparam S
  *   the source of a [[Iso]]
  * @tparam T
  *   the focus of a [[Iso]]
  */
@implicitNotFound("Could not find an instance of Reverse[${S}, ${T}]")
trait Reverse[S, T] extends Serializable {
  def reverse: Iso[S, T]
}

object Reverse {
  /** summon an instance of [[Reverse]] */
  @inline def apply[S, T](implicit ev: Reverse[S, T]): Reverse[S, T] = ev
}
