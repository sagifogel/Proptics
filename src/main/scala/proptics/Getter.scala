package proptics

import proptics.Fold.liftForget
import proptics.internal.Forget

/**
 * A [[Getter]] is a [[Fold]]
 *
 * @tparam S the source of a [[Getter]]
 * @tparam T the modified source of a [[Getter]]
 * @tparam A the target of a [[Getter]]
 * @tparam B the modified target of a [[Getter]]
 */
abstract class Getter[S, T, A, B] extends Serializable {
  def apply[R](forget: Forget[R, A, B]): Forget[R, S, T]
}

object Getter {
  def apply[S, T, A, B](f: S => A): Getter[S, T, A, B] = new Getter[S, T, A, B] {
    override def apply[R](forget: Forget[R, A, B]): Forget[R, S, T] = liftForget[R, S, T, A, B](f)(forget)
  }

  def to[S, T, A, B](f: S => A): Getter[S, T, A, B] = Getter(f)
}

object Getter_ {
  def apply[S, A](f: S => A): Getter_[S, A] = to(f)

  def to[S, T, A, B](f: S => A): Getter_[S, A] = Getter(f)
}
