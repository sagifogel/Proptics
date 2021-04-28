package proptics.typeclass

import scala.annotation.implicitNotFound

import proptics.instances.field1._
import proptics.instances.field2._
import proptics.std.tuple._
import proptics.{Iso, Lens}

/** The [[NonEmptyCons]] typeclass provides an [[Iso]] for a way to attach or detach elements on the left side of a structure.
  *
  * The [[NonEmptyCons]] is similar to [[Cons]], but is suitable for Non empty structures.
  *
  * @tparam S the source of an [[Iso]]
  * @tparam H the head part of the focus `(H, T)` of an [[Iso]]
  * @tparam T the tail part of the focus `(H, T)` of an [[Iso]]
  */
@implicitNotFound("Could not find an instance of NonEmptyCons[${S}, ${H}, ${T}]")
trait NonEmptyCons[S, H, T] extends Serializable {
  def nonEmptyCons: Iso[S, (H, T)]

  def head: Lens[S, H] = nonEmptyCons compose _1[H, T]

  def tail: Lens[S, T] = nonEmptyCons compose _2[H, T]
}

object NonEmptyCons {
  /** summon an instance of [[NonEmptyCons]] */
  @inline def apply[S, H, T](implicit ev: NonEmptyCons[S, H, T]): NonEmptyCons[S, H, T] = ev
}
