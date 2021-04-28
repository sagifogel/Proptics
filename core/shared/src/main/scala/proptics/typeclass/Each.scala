package proptics.typeclass

import scala.annotation.implicitNotFound

import proptics.Traversal

/** The [[Each]] typeclass provides a [[Traversal]] that can extract each element of a monomorphic container.
  *
  * @tparam S the source of a [[Traversal]]
  * @tparam A the focus of a [[Traversal]]
  */
@implicitNotFound("Could not find an instance of Each[${S}, ${A}]")
trait Each[S, A] extends Serializable {
  def each: Traversal[S, A]
}

object Each {
  /** summon an instance of [[Each]] */
  @inline def apply[S, A](implicit ev: Each[S, A]): Each[S, A] = ev
}
