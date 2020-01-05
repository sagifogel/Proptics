package optics

import cats.syntax.either._
import optics.internal.Forget
import optics.profunctor.Choice

/**
 * A [[Fold]] is an [[Optic]] with fixed type [[Forget]] [[cats.arrow.Profunctor]]
 *
 * @tparam R the return type of a [[Fold]]
 * @tparam S the source of a [[Fold]]
 * @tparam T the modified source of a [[Fold]]
 * @tparam A the target of a [[Fold]]
 * @tparam B the modified target of a [[Fold]]
 */
abstract class Fold[R, S, T, A, B] extends Optic[Forget[R, *, *], S, T, A, B] {
}

object Fold {
  private[optics] def liftForget[R, S, T, A, B](f: S => A): Forget[R, A, B] => Forget[R, S, T] =
    forget => Forget[R, S, T](forget.runForget compose f)

  private[optics] def apply[R, S, T, A, B](f: Forget[R, A, B] => Forget[R, S, T]): Fold[R, S, T, A, B] = new Fold[R, S, T, A, B] {
    override def apply(pab: Forget[R, A, B]): Forget[R, S, T] = f(pab)
  }

  def apply[R, S, T, A, B](f: S => A)(implicit ev: DummyImplicit): Fold[R, S, T, A, B] =
    Fold(liftForget[R, S, T, A, B](f))

  def filtered[P[_, _], A](predicate: A => Boolean)(implicit ev: Choice[P]): Optic_[P, A, A] =
    Optic_[P, A, A](pab => ev.dimap[Either[A, A], Either[A, A], A, A](ev.right(pab))
      (x => if (predicate(x)) x.asRight[A] else x.asLeft[A])(_.fold(identity[A], identity[A])))

}
