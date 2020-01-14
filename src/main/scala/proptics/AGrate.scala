package proptics

import proptics.internal.Grating
import proptics.profunctor.Closed

/**
 * An [[Optic] with fixed type [[Grating]] [[cats.arrow.Profunctor]]
 *
 * @tparam S the source of an [[AGrate]]
 * @tparam T the modified source of an [[AGrate]]
 * @tparam A the target of an [[AGrate]]
 * @tparam B the modified target of an [[AGrate]]
 */
abstract class AGrate[S, T, A, B] extends Optic[Grating[A, B, *, *], S, T, A, B] { self =>
  def cloneGrate[P[_, _]: Closed]: Grate[P, S, T, A, B] = Grate(f => withGrate(f))

  def withGrate(f: (S => A) => B): T = self(Grating(f => f(identity[A]))).runGrating(f)
}
