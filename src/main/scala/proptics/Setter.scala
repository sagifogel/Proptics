package proptics

import cats.{Contravariant, Functor}

import scala.Function.const

/**
 * A [[Setter]] is an [[Optic]] with a fixed type of a [[Function1]] as the type constructor
 *
 * @tparam S the source of a [[Setter]]
 * @tparam T the modified source of a [[Setter]]
 * @tparam A the target of a [[Setter]]
 * @tparam B the modified target of a [[Setter]]
 */
abstract class Setter[S, T, A, B] extends Serializable { self =>
  private[proptics] def apply(pab: A => B): S => T

  def over(f: A => B): S => T = self(f)

  def set(b: B): S => T = over(const(b))
}

object Setter {
  def apply[S, T, A, B](f: (A => B) => S => T): Setter[S, T, A, B] = new Setter[S, T, A, B] {
    override def apply(pab: A => B): S => T = f(pab)
  }

  def fromFunctor[F[_], A, B](implicit ev: Functor[F]): Setter[F[A], F[B], A, B] = Setter(ev.lift)

  def fromContravariant[F[_], A, B](implicit ev: Contravariant[F]): Setter[F[B], F[A], A, B] =
    Setter(ev.liftContravariant)
}

object Setter_ {
  def apply[S, A](f: (A => A) => S => S): Setter[S, S, A, A] = Setter[S, S, A, A](f)
}