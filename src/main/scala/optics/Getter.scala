package optics

import optics.internal.Forget

/**
 * A [[Getter]] is a [[Fold]]
 *
 * @tparam R the return type of a [[Getter]]
 * @tparam S the source of a [[Getter]]
 * @tparam T the modified source of a [[Getter]]
 * @tparam A the target of a [[Getter]]
 * @tparam B the modified target of a [[Getter]]
 */
abstract class Getter[R, S, T, A, B] extends Fold[R, S, T, A, B] {
}

object Getter {
  private[Getter] def apply[R, S, T, A, B](f: Forget[R, A, B] => Forget[R, S, T]): Getter[R, S, T, A, B] = new Getter[R, S, T, A, B] {
    override def apply(pab: Forget[R, A, B]): Forget[R, S, T] = f(pab)
  }

  def apply[R, S, T, A, B](f: S => A)(implicit ev: DummyImplicit): Getter[R, S, T, A, B] = {
    Getter(Fold.liftForget[R, S, T, A, B](f))
  }

  def to[R, S, T, A, B](f: S => A): Getter[R, S, T, A, B] = Getter(f)
}
