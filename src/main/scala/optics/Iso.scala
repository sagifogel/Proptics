package optics

import cats.arrow.Profunctor

/**
 * A generalized isomorphism
 *
 * @tparam P an evidence of [[Profunctor]]
 * @tparam S the source of an [[Iso]]
 * @tparam T the modified source of an [[Iso]]
 * @tparam A the target of a [[Iso]]
 * @tparam B the modified target of a [[Iso]]
 */
abstract class Iso[P[_, _]: Profunctor, S, T, A, B] extends Optic[P, S, T, A, B] { self =>
}

object Iso {
  private[optics] def apply[P[_, _], S, T, A, B](f: P[A, B] => P[S, T])(implicit ev: Profunctor[P]): Iso[P, S, T, A, B] = new Iso[P, S, T, A, B] {
    override def apply(pab: P[A, B]): P[S, T] = f(pab)
  }

  def apply[P[_, _], S, T, A, B](get: S => A)(inverseGet: B => T)(implicit ev: Profunctor[P]): Iso[P, S, T, A, B] = {
    Iso(ev.dimap(_)(get)(inverseGet))
  }
}
