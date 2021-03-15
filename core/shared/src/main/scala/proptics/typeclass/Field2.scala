package proptics.typeclass

import scala.annotation.implicitNotFound

import proptics.Lens

@implicitNotFound("Could not find an instance of Field2[${S}, ${A}]")
trait Field2[S, A] extends Serializable {
  def second: Lens[S, A]
}

object Field2 {
  /** summon an instance of [[Field2]] */
  @inline def apply[S, A](implicit ev: Field2[S, A]): Field2[S, A] = ev
}
