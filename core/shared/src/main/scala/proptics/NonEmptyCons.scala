package proptics

import scala.annotation.implicitNotFound

import proptics.std.tuple._

@implicitNotFound("Could not find an instance of NonEmptyCons[${S}, ${A}]")
trait NonEmptyCons[S, A] extends Serializable {
  def cons: Iso[S, (A, S)]

  def head: Lens[S, A] = cons compose _1[A, S]

  def tail: Lens[S, S] = cons compose _2[A, S]
}

object NonEmptyCons {
  /** summon an instance of [[NonEmptyCons]] */
  @inline def apply[S, A](implicit ev: NonEmptyCons[S, A]): NonEmptyCons[S, A] = ev
}
