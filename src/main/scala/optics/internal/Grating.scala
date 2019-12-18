package optics.internal

import cats.arrow.Profunctor
import optics.profunctor.Closed

final case class Grating[A, B, S, T](runGrating: ((S => A) => B) => T)

abstract class GratingInstances {
  def profunctorGrating[E, F]: Profunctor[Grating[E, F, *, *]] = new Profunctor[Grating[E, F, *, *]] {
    override def dimap[A, B, C, D](fab: Grating[E, F, A, B])(f: C => A)(g: B => D): Grating[E, F, C, D] =
      Grating(c2e2f => g(fab.runGrating(a2e => c2e2f(a2e compose f))))
  }

  def closedGrating[E, F]: Closed[Grating[E, F, *, *]] = new Closed[Grating[E, F, *, *]] {
    override def closed[A, B, C](pab: Grating[E, F, A, B]): Grating[E, F, C => A, C => B] =
      Grating(c2a2e2f => c => {
        pab.runGrating(a2e => c2a2e2f(c2a => a2e(c2a(c))))
      })

    override def dimap[A, B, C, D](fab: Grating[E, F, A, B])(f: C => A)(g: B => D): Grating[E, F, C, D] =
      profunctorGrating.dimap(fab)(f)(g)
  }
}

object Grating extends GratingInstances