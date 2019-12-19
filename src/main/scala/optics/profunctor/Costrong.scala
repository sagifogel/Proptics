package optics.profunctor

import cats.arrow.Profunctor

/** The [[Costrong]] class provides the dual operations of the [[Strong]] class. */
trait Costrong[P[_, _]] extends Profunctor[P] {
  def unfirst[A, B, C](p: P[(A, C), (B, C)]): P[A, B]

  def unsecond[A, B, C](p: P[(A, B), (A, C)]): P[B, C]
}
