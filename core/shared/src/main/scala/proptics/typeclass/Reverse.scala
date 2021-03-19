package proptics.typeclass

import scala.annotation.implicitNotFound

import proptics.Iso

@implicitNotFound("Could not find an instance of Reverse[${S}, ${T}]")
trait Reverse[S, T] extends Serializable {
  def reverse: Iso[S, T]
}

object Reverse {
  def apply[S, T](implicit ev: Reverse[S, T]): Reverse[S, T] = ev
}
