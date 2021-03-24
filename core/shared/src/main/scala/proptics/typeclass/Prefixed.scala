package proptics.typeclass

import scala.annotation.implicitNotFound

import proptics.Prism

@implicitNotFound("Could not find an instance of Prefixed[${S}, ${T}]")
trait Prefixed[S, T] extends Serializable {
  def prefixed(s: S): Prism[S, T]
}

object Prefixed {
  /** summon an instance of [[Prefixed]] */
  @inline def apply[S, T](implicit ev: Prefixed[S, T]): Prefixed[S, T] = ev
}
