package proptics.profunctor

import scala.annotation.implicitNotFound

import cats.arrow.Profunctor

/** The [[Closed]] type class extends the Profunctor class to work with functions.
 *
  * A [[cats.arrow.Strong]] [[cats.arrow.Profunctor]] allows the monoidal structure to pass through.
 *
  * A [[Closed]] [[cats.arrow.Profunctor]] allows the closed structure to pass through
  */
@implicitNotFound("Could not find an instance of Closed[${P}]")
trait Closed[P[_, _]] extends Profunctor[P] {
  def closed[A, B, C](pab: P[A, B]): P[C => A, C => B]
}

abstract class ClosedInstances {
  implicit final def closedFunction: Closed[* => *] = new Closed[* => *] {
    override def closed[A, B, C](pab: A => B): (C => A) => C => B = c2a => pab compose c2a

    override def dimap[A, B, C, D](fab: A => B)(f: C => A)(g: B => D): C => D = g compose fab compose f
  }
}

object Closed extends ClosedInstances {
  /** summon an instance of [[Closed]] for `P` */
  @inline def apply[P[_, _]](implicit ev: Closed[P]): Closed[P] = ev
}
