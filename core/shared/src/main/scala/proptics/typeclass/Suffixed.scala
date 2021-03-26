package proptics.typeclass

import scala.annotation.implicitNotFound

import proptics.Prism

@implicitNotFound("Could not find an instance of Suffixed[${S}, ${T}]")
trait Suffixed[S, T] extends Serializable {
  def suffixed(s: S): Prism[S, T]
}

object Suffixed {
  /** summon an instance of [[Suffixed]] */
  @inline def apply[S, T](implicit ev: Suffixed[S, T]): Suffixed[S, T] = ev
}
