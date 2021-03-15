package proptics.typeclass

import scala.annotation.implicitNotFound

import proptics.Lens

@implicitNotFound("Could not find an instance of Field3[${S}, ${A}]")
trait Field3[S, A] extends Serializable {
  def third: Lens[S, A]
}

object Field3 {
  /** summon an instance of [[Field3]] */
  @inline def apply[S, A](implicit ev: Field3[S, A]): Field3[S, A] = ev
}
