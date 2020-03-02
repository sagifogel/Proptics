package proptics

import proptics.internal.Grating

/**
 * An Optic with fixed type [[Grating [[cats.arrow.Profunctor]]
 *
 * @tparam S the source of an [[AGrate]]
 * @tparam T the modified source of an [[AGrate]]
 * @tparam A the target of an [[AGrate]]
 * @tparam B the modified target of an [[AGrate]]
 */
abstract class AGrate[S, T, A, B] { self =>
  def apply(grating: Grating[A, B, A, B]): Grating[A, B, S, T]

  def cloneGrate: Grate[S, T, A, B] = Grate(withGrate _)

  def withGrate(f: (S => A) => B): T = self(Grating(_.apply(identity))).runGrating(f)
}

object AGrate {
  private[proptics] def apply[S, T, A, B](f: Grating[A, B, A, B] => Grating[A, B, S, T]): AGrate[S, T, A, B] = new AGrate[S, T, A, B] {
    override def apply(grating: Grating[A, B, A, B]): Grating[A, B, S, T] = f(grating)
  }

  def apply[S, T, A, B](to: ((S => A) => B) => T)(implicit ev: DummyImplicit): AGrate[S, T, A, B] = AGrate((_: Grating[A, B, A, B]) => Grating(to))
}


object AGrate_ {
  def apply[S, A](to: ((S => A) => A) => S): AGrate_[S, A] = AGrate(to)
}
