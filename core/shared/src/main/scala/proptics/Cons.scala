package proptics

import scala.annotation.implicitNotFound

import proptics.std.tuple._

@implicitNotFound("Could not find an instance of Cons[${S}, ${A}]")
trait Cons[S, A] extends Serializable {
  def cons: Prism[S, (A, S)]

  def headOption: AffineTraversal[S, A] = cons compose _1[A, S]

  def tailOption: AffineTraversal[S, S] = cons compose _2[A, S]
}

object Cons {
  /** summon an instance of [[Cons]] */
  @inline def apply[S, A](implicit ev: Cons[S, A]): Cons[S, A] = ev
}
