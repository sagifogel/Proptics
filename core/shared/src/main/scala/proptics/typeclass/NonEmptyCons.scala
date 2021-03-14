package proptics.typeclass

import scala.annotation.implicitNotFound

import proptics.std.tuple._
import proptics.{Iso, Lens}

@implicitNotFound("Could not find an instance of NonEmptyCons[${S}, ${A}]")
trait NonEmptyCons[S, H, T] extends Serializable {
  def nonEmptyCons: Iso[S, (H, T)]

  def head: Lens[S, H] = nonEmptyCons compose _1[H, T]

  def tail: Lens[S, T] = nonEmptyCons compose _2[H, T]
}

object NonEmptyCons {
  /** summon an instance of [[NonEmptyCons]] */
  @inline def apply[S, H, T](implicit ev: NonEmptyCons[S, H, T]): NonEmptyCons[S, H, T] = ev
}
