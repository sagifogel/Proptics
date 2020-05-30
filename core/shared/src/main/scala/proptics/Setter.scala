package proptics

import cats.instances.function._
import cats.{Contravariant, Functor}

import scala.Function.const

/**
  * A [[Setter_]] A Setter is a generalization of fmap from [[Functor]]
  *
  * @tparam S the source of a [[Setter_]]
  * @tparam T the modified source of a [[Setter_]]ß
  * @tparam A the focus of a [[Setter_]]
  * @tparam B the modified focus of a [[Setter_]]
  */
abstract class Setter_[S, T, A, B] extends Serializable { self =>
  private[proptics] def apply(pab: A => B): S => T

  /** set the modified focus of a [[Setter_]] */
  def set(b: B): S => T = over(const(b))

  /** modify the focus type of a [[Setter_]] using a function, resulting in a change of type to the full structure  */
  def over(f: A => B): S => T = self(f)

  /** compose [[Setter_]] with an [[Iso_]] */
  def compose[C, D](other: Iso_[A, B, C, D]): Setter_[S, T, C, D] = new Setter_[S, T, C, D] {
    override private[proptics] def apply(pab: C => D) = self(other(pab))
  }

  /** compose [[Setter_]] with an [[AnIso_]] */
  def compose[C, D](other: AnIso_[A, B, C, D]): Setter_[S, T, C, D] = self compose other.asIso

  /** compose [[Setter_]] with a [[Lens_]] */
  def compose[C, D](other: Lens_[A, B, C, D]): Setter_[S, T, C, D] = new Setter_[S, T, C, D] {
    override private[proptics] def apply(pab: C => D): S => T = self(other(pab))
  }

  /** compose [[Setter_]] with an [[ALens_]] */
  def compose[C, D](other: ALens_[A, B, C, D]): Setter_[S, T, C, D] = self compose other.asLens

  /** compose [[Setter_]] with a [[Prism_]] */
  def compose[C, D](other: Prism_[A, B, C, D]): Setter_[S, T, C, D] = new Setter_[S, T, C, D] {
    override private[proptics] def apply(pab: C => D): S => T = self(other(pab))
  }

  /** compose [[Setter_]] with an [[APrism_]] */
  def compose[C, D](other: APrism_[A, B, C, D]): Setter_[S, T, C, D] = self compose other.asPrism

  /** compose [[Setter_]] with a [[Traversal_]] */
  def compose[C, D](other: Traversal_[A, B, C, D]): Setter_[S, T, C, D] = new Setter_[S, T, C, D] {
    override private[proptics] def apply(pab: C => D): S => T = self(other(pab))
  }

  /** compose [[Setter_]] with an [[ATraversal_]] */
  def compose[C, D](other: ATraversal_[A, B, C, D]): Setter_[S, T, C, D] = self compose other.asTraversal

  /** compose [[Setter_]] with a [[Setter_]] */
  def compose[C, D](other: Setter_[A, B, C, D]): Setter_[S, T, C, D] = new Setter_[S, T, C, D] {
    override private[proptics] def apply(pab: C => D) = self(other(pab))
  }

  /** compose [[Setter_]] with a [[Grate_]] */
  def compose[C, D](other: Grate_[A, B, C, D]): Setter_[S, T, C, D] = new Setter_[S, T, C, D] {
    override private[proptics] def apply(pab: C => D): S => T = self(other(pab))
  }
}

object Setter_ {

  /** create a polymorphic setter from a [[Setter_.over]] function */
  def apply[S, T, A, B](over: (A => B) => S => T): Setter_[S, T, A, B] = new Setter_[S, T, A, B] {
    override def apply(pab: A => B): S => T = over(pab)
  }

  /** create a polymorphic setter from a [[Functor]] */
  def fromFunctor[F[_], A, B](implicit ev: Functor[F]): Setter_[F[A], F[B], A, B] = Setter_(ev.lift)

  /** create a polymorphic setter from a [[Contravariant]] */
  def fromContravariant[F[_], A, B](implicit ev: Contravariant[F]): Setter_[F[B], F[A], A, B] =
    Setter_(ev.liftContravariant)
}

object Setter {

  /** create a monomorphic setter from a [[Setter_.over]] function */
  def apply[S, A](over: (A => A) => S => S): Setter_[S, S, A, A] = Setter_[S, S, A, A](over)

  /** create a monomorphic setter from a [[Functor]] */
  def fromFunctor[F[_], A](implicit ev: Functor[F]): Setter_[F[A], F[A], A, A] = Setter_(ev.lift)

  /** create a monomorphic setter from a [[Contravariant]] */
  def fromContravariant[F[_], A](implicit ev: Contravariant[F]): Setter_[F[A], F[A], A, A] =
    Setter_(ev.liftContravariant)
}
