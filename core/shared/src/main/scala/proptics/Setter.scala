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

  /** modify the focus type of a [[Setter_]] using a function, resulting in a change of type to the full structure */
  def over(f: A => B): S => T = self(f)

  /** compose a [[Setter_]] with an [[Iso_]] */
  def compose[C, D](other: Iso_[A, B, C, D]): Setter_[S, T, C, D] = new Setter_[S, T, C, D] {
    override private[proptics] def apply(pab: C => D): S => T = self(other(pab))
  }

  /** compose a [[Setter_]] with an [[AnIso_]] */
  def compose[C, D](other: AnIso_[A, B, C, D]): Setter_[S, T, C, D] = self compose other.asIso

  /** compose a [[Setter_]] with a [[Lens_]] */
  def compose[C, D](other: Lens_[A, B, C, D]): Setter_[S, T, C, D] = new Setter_[S, T, C, D] {
    override private[proptics] def apply(pab: C => D): S => T = self(other(pab))
  }

  /** compose a [[Setter_]] with an [[ALens_]] */
  def compose[C, D](other: ALens_[A, B, C, D]): Setter_[S, T, C, D] = self compose other.asLens

  /** compose a [[Setter_]] with a [[Prism_]] */
  def compose[C, D](other: Prism_[A, B, C, D]): Setter_[S, T, C, D] = new Setter_[S, T, C, D] {
    override private[proptics] def apply(pab: C => D): S => T = self(other(pab))
  }

  /** compose a [[Setter_]] with an [[APrism_]] */
  def compose[C, D](other: APrism_[A, B, C, D]): Setter_[S, T, C, D] = self compose other.asPrism

  /** compose a [[Setter_]] with a [[AffineTraversal_]] */
  def compose[C, D](other: AffineTraversal_[A, B, C, D]): Setter_[S, T, C, D] = new Setter_[S, T, C, D] {
    override private[proptics] def apply(pab: C => D): S => T = self(other(pab))
  }

  /** compose a [[Setter_]] with an [[AnAffineTraversal_]] */
  def compose[C, D](other: AnAffineTraversal_[A, B, C, D]): Setter_[S, T, C, D] = new Setter_[S, T, C, D] {
    override private[proptics] def apply(pab: C => D): S => T = self(other.over(pab))
  }

  /** compose a [[Setter_]] with a [[Traversal_]] */
  def compose[C, D](other: Traversal_[A, B, C, D]): Setter_[S, T, C, D] = new Setter_[S, T, C, D] {
    override private[proptics] def apply(pab: C => D): S => T = self(other(pab))
  }

  /** compose a [[Setter_]] with an [[ATraversal_]] */
  def compose[C, D](other: ATraversal_[A, B, C, D]): Setter_[S, T, C, D] = self compose other.asTraversal

  /** compose a [[Setter_]] with a [[Setter_]] */
  def compose[C, D](other: Setter_[A, B, C, D]): Setter_[S, T, C, D] = new Setter_[S, T, C, D] {
    override private[proptics] def apply(pab: C => D): S => T = self(other(pab))
  }

  /** compose a [[Setter_]] with a [[Grate_]] */
  def compose[C, D](other: Grate_[A, B, C, D]): Setter_[S, T, C, D] = new Setter_[S, T, C, D] {
    override private[proptics] def apply(pab: C => D): S => T = self(other(pab))
  }
}

object Setter_ {

  /** create a polymorphic setter from a mapping function */
  def apply[S, T, A, B](mapping: (A => B) => S => T): Setter_[S, T, A, B] = new Setter_[S, T, A, B] {
    override def apply(pab: A => B): S => T = mapping(pab)
  }

  /** create a polymorphic setter from a [[Functor]] */
  def fromFunctor[F[_], A, B](implicit ev: Functor[F]): Setter_[F[A], F[B], A, B] = Setter_(ev.lift)

  /** create a polymorphic setter from a [[Contravariant]] */
  def fromContravariant[F[_], A, B](implicit ev: Contravariant[F]): Setter_[F[B], F[A], A, B] =
    Setter_(ev.liftContravariant)

  /** polymorphic identity of a [[Setter_]] */
  def id[S, T]: Setter_[S, T, S, T] = Setter_[S, T, S, T](st => st)
}

object Setter {

  /** create a monomorphic setter from a mapping function */
  def apply[S, A](mapping: (A => A) => S => S): Setter[S, A] = Setter_[S, S, A, A](mapping)

  /** create a monomorphic setter from a [[Functor]] */
  def fromFunctor[F[_], A](implicit ev: Functor[F]): Setter[F[A], A] = Setter_(ev.lift)

  /** create a monomorphic setter from a [[Contravariant]] */
  def fromContravariant[F[_], A](implicit ev: Contravariant[F]): Setter[F[A], A] =
    Setter_(ev.liftContravariant)

  /** monomorphic identity of a [[Setter]] */
  def id[S]: Setter[S, S] = Setter_.id[S, S]
}
