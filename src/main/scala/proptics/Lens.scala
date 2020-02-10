package proptics

import cats.arrow.Strong
import cats.instances.function._
import cats.syntax.apply._
import proptics.rank2types.Rank2TypeLensLike

/**
 * Given a type whose "focus element" always exists,
 * a [[Lens]] provides a convenient way to view, set, and transform
 * that element.
 *
 * @tparam S the source of a [[Lens]]
 * @tparam T the modified source of a [[Lens]]
 * @tparam A the target of a [[Lens]]
 * @tparam B the modified target of a [[Lens]]
 */
abstract class Lens[S, T, A, B] {
  def apply[P[_, _]](pab: P[A, B])(implicit ev: Strong[P]): P[S, T]
}

object Lens {
  private[proptics] def apply[S, T, A, B](f: Rank2TypeLensLike[S, T, A, B]): Lens[S, T, A, B] = new Lens[S, T, A, B] {
    override def apply[P[_, _]](pab: P[A, B])(implicit ev: Strong[P]): P[S, T] = f(pab)
  }

  /**
   * Create a [[Lens]] from a getter/setter pair.
   */
  def apply[S, T, A, B](get: S => A)(set: S => B => T): Lens[S, T, A, B] =
    lens((get, set).mapN(Tuple2.apply))

  def lens[S, T, A, B](to: S => (A, B => T)): Lens[S, T, A, B] =
    Lens(new Rank2TypeLensLike[S, T, A, B] {
      override def apply[P[_, _]](pab: P[A, B])(implicit ev: Strong[P]): P[S, T] =
        liftOptic(to)(ev)(pab)
    })

  private[proptics] def liftOptic[P[_, _], S, T, A, B](to: S => (A, B => T))(implicit ev: Strong[P]): P[A, B] => P[S, T] =
    pab => {
      val first = ev.first[A, B, B => T](pab)
      ev.dimap(first)(to) { case (b, f) => f(b) }
    }
}

object Lens_ {
  def apply[S, A](get: S => A)(set: S => A => S): Lens_[S, A] = Lens[S, S, A, A](get)(set)

  def lens[S, A](to: S => (A, A => S)): Lens_[S, A] = Lens.lens[S, S, A, A](to)
}
