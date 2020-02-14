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
abstract class AGrate[S, T, A, B] { self =>
  def apply[P[_, _]](grating: Grating[A, B, A, B])(implicit ev: Closed[P]): Grating[A, B, S, T]

  def cloneGrate[P[_, _] : Closed]: Grate[P, S, T, A, B] = Grate(f => withGrate(f))

  def withGrate(f: (S => A) => B): T = self.apply(Grating(_.apply(identity))).runGrating(f)
}

object AGrate {
  def apply[S, T, A, B](f: Grating[A, B, A, B] => Grating[A, B, S, T]): AGrate[S, T, A, B] = new AGrate[S, T, A, B] {
    override def apply[P[_, _]](grating: Grating[A, B, A, B])(implicit ev: Closed[P]): Grating[A, B, S, T] = f(grating)
  }
}
