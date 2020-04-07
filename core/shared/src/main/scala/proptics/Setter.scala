package proptics

import cats.{Contravariant, Functor}

import scala.Function.const

/**
  * A [[Setter_]] A Setter is a generalization of fmap from [[Functor]]
  *
  * @tparam S the source of a [[Setter_]]
  * @tparam T the modified source of a [[Setter_]]
  * @tparam A the target of a [[Setter_]]
  * @tparam B the modified target of a [[Setter_]]
  */
abstract class Setter_[S, T, A, B] extends Serializable { self =>
  private[proptics] def apply(pab: A => B): S => T

  def set(b: B): S => T = over(const(b))

  def over(f: A => B): S => T = self(f)
}

object Setter_ {
  def apply[S, T, A, B](f: (A => B) => S => T): Setter_[S, T, A, B] = new Setter_[S, T, A, B] {
    override def apply(pab: A => B): S => T = f(pab)
  }

  def fromFunctor[F[_], A, B](implicit ev: Functor[F]): Setter_[F[A], F[B], A, B] = Setter_(ev.lift)

  def fromContravariant[F[_], A, B](implicit ev: Contravariant[F]): Setter_[F[B], F[A], A, B] =
    Setter_(ev.liftContravariant)
}

object Setter {
  def apply[S, A](f: (A => A) => S => S): Setter_[S, S, A, A] = Setter_[S, S, A, A](f)
}
