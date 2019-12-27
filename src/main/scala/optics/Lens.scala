package optics

import cats.arrow.Strong
import cats.instances.function._
import cats.syntax.apply._

/**
 * Given a type whose "focus element" always exists,
 * a [[Lens]] provides a convenient way to view, set, and transform
 * that element.
 *
 * @tparam P an evidence of [[Strong]] [[cats.arrow.Profunctor]]
 * @tparam S the source of a [[Lens]]
 * @tparam T the modified source of a [[Lens]]
 * @tparam A the target of a [[Lens]]
 * @tparam B the modified target of a [[Lens]]
 */
abstract class Lens[P[_, _] : Strong, S, T, A, B] extends Optic[P, S, T, A, B] {
}

object Lens {
  private[optics] def apply[P[_, _], S, T, A, B](f: P[A, B] => P[S, T])(implicit ev: Strong[P]): Lens[P, S, T, A, B] =
    new Lens[P, S, T, A, B] {
      override def apply(pab: P[A, B]): P[S, T] = f(pab)
    }

  /**
   * Create a [[Lens]] from a getter/setter pair.
   */
  def apply[P[_, _], S, T, A, B](get: S => A)(set: S => B => T)(implicit ev: Strong[P]): Lens[P, S, T, A, B] =
    lens((get, set).mapN(Tuple2.apply))

  def lens[P[_, _], S, T, A, B](to: S => (A, B => T))(implicit ev: Strong[P]): Lens[P, S, T, A, B] =
    Lens(pab => {
      val first = ev.first[A, B, B => T](pab)
      ev.dimap(first)(to) { case (b, f) => f(b) }
    })
}

object Lens_ {
  def apply[P[_, _], S, A](get: S => A)(set: S => A => S)(implicit ev: Strong[P]): Lens_[P, S, A] =
    Lens[P, S, S, A, A](get)(set)

  def lens[P[_, _], S, A](to: S => (A, A => S))(implicit ev: Strong[P]): Lens_[P, S, A] =
    Lens.lens[P, S, S, A, A](to)
}
