package proptics.typeclass

import scala.annotation.implicitNotFound

import proptics.Lens

@implicitNotFound("Could not find an instance of Field1[${S}, ${A}]")
trait Field1[S, A] extends Serializable {
  def first: Lens[S, A]
}

object Field1 {
  /** summon an instance of [[Field1]] */
  @inline def apply[S, A](implicit ev: Field1[S, A]): Field1[S, A] = ev
}
