package optics

import cats.arrow.Strong

/**
 * Given a type whose "focus element" always exists,
 * a [[Lens]] provides a convenient way to view, set, and transform
 * that element.
 *
 * @tparam P an evidence of [[Strong]] [[Profunctor]]
 * @tparam S the source of a [[Lens]]
 * @tparam T the modified source of a [[Lens]]
 * @tparam A the target of a [[Lens]]
 * @tparam B the modified target of a [[Lens]]
 */
abstract class Lens[P[_, _]: Strong , S, T, A, B] extends Optic[P, S, T, A, B] { self =>
}

object Lens {
  def apply[P[_, _], S, T, A, B](f: P[A, B] => P[S, T])(implicit ev: Strong[P]): Lens[P, S, T, A, B] =
    new Lens[P, S, T, A, B] {
      val pab: P[A, B] => P[S, T] = f
    }

  /**
   * Create a [[Lens]] from a getter/setter pair.
   */
  def lens[P[_, _], S, T, A, B](get: S => A)(set: S => B => T)(implicit ev: Strong[P]): Lens[P, S, T, A, B]  =
    lens_(s => (get(s), set(s)))

  def lens_[P[_, _], S, T, A, B](to: S => (A, B => T))(implicit ev: Strong[P]): Lens[P, S, T, A, B] =
    Lens(pab => {
      val first = ev.first[A, B, B => T](pab)
      ev.dimap(first)(to) { case (b, f) => f(b) }
    })
}

object Lens_ {
  def apply[P[_, _], S, A](f: P[A, A] => P[S, S])(implicit ev: Strong[P]): Lens_[P, S, A] = Lens(f)
}