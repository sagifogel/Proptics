package proptics
import scala.Function.const

import cats.{Contravariant, Functor}

import proptics.internal.Indexed

/** A [[Setter_]] is a generalization of map from [[cats.Functor]]
  *
  * @tparam S the source of a [[Setter_]]
  * @tparam T the modified source of a [[Setter_]]ß
  * @tparam A the focus of a [[Setter_]]
  * @tparam B the modified focus of a [[Setter_]]
  */
abstract class Setter_[S, T, A, B] extends Serializable { self =>
  private[proptics] def apply(pab: A => B): S => T

  /** set the modified focus of a [[Setter_]] */
  final def set(b: B): S => T = over(const(b))

  /** modify the focus type of a [[Setter_]] using a function, resulting in a change of type to the full structure */
  final def over(f: A => B): S => T = self(f)

  /** compose a [[Setter_]] with an [[Iso_]] */
  final def compose[C, D](other: Iso_[A, B, C, D]): Setter_[S, T, C, D] = new Setter_[S, T, C, D] {
    override private[proptics] def apply(pab: C => D): S => T = self(other(pab))
  }

  /** compose a [[Setter_]] with an [[AnIso_]] */
  final def compose[C, D](other: AnIso_[A, B, C, D]): Setter_[S, T, C, D] = self compose other.asIso

  /** compose a [[Setter_]] with a [[Lens_]] */
  final def compose[C, D](other: Lens_[A, B, C, D]): Setter_[S, T, C, D] = new Setter_[S, T, C, D] {
    override private[proptics] def apply(pab: C => D): S => T = self(other(pab))
  }

  /** compose a [[Setter_]] with an [[ALens_]] */
  final def compose[C, D](other: ALens_[A, B, C, D]): Setter_[S, T, C, D] = self compose other.asLens

  /** compose a [[Setter_]] with a [[Prism_]] */
  final def compose[C, D](other: Prism_[A, B, C, D]): Setter_[S, T, C, D] = new Setter_[S, T, C, D] {
    override private[proptics] def apply(pab: C => D): S => T = self(other(pab))
  }

  /** compose a [[Setter_]] with an [[APrism_]] */
  final def compose[C, D](other: APrism_[A, B, C, D]): Setter_[S, T, C, D] = self compose other.asPrism

  /** compose a [[Setter_]] with a [[AffineTraversal_]] */
  final def compose[C, D](other: AffineTraversal_[A, B, C, D]): Setter_[S, T, C, D] = new Setter_[S, T, C, D] {
    override private[proptics] def apply(pab: C => D): S => T = self(other(pab))
  }

  /** compose a [[Setter_]] with an [[AnAffineTraversal_]] */
  final def compose[C, D](other: AnAffineTraversal_[A, B, C, D]): Setter_[S, T, C, D] = new Setter_[S, T, C, D] {
    override private[proptics] def apply(pab: C => D): S => T = self(other.over(pab))
  }

  /** compose a [[Setter_]] with a [[Traversal_]] */
  final def compose[C, D](other: Traversal_[A, B, C, D]): Setter_[S, T, C, D] = new Setter_[S, T, C, D] {
    override private[proptics] def apply(pab: C => D): S => T = self(other(pab))
  }

  /** compose a [[Setter_]] with an [[ATraversal_]] */
  final def compose[C, D](other: ATraversal_[A, B, C, D]): Setter_[S, T, C, D] = self compose other.asTraversal

  /** compose a [[Setter_]] with a [[Setter_]] */
  final def compose[C, D](other: Setter_[A, B, C, D]): Setter_[S, T, C, D] = new Setter_[S, T, C, D] {
    override private[proptics] def apply(pab: C => D): S => T = self(other(pab))
  }

  /** compose a [[Setter_]] with a [[Grate_]] */
  final def compose[C, D](other: Grate_[A, B, C, D]): Setter_[S, T, C, D] = new Setter_[S, T, C, D] {
    override private[proptics] def apply(pab: C => D): S => T = self(other(pab))
  }

  /** compose a [[Setter_]] with an [[IndexedLens_]] */
  final def compose[I, C, D](other: IndexedLens_[I, A, B, C, D]): IndexedSetter_[I, S, T, C, D] = new IndexedSetter_[I, S, T, C, D] {
    override private[proptics] def apply(indexed: Indexed[* => *, I, C, D]): S => T =
      self.over(other.over(indexed.runIndex))
  }

  /** compose a [[Setter_]] with an [[AnIndexedLens_]] */
  final def compose[I, C, D](other: AnIndexedLens_[I, A, B, C, D]): IndexedSetter_[I, S, T, C, D] = new IndexedSetter_[I, S, T, C, D] {
    override private[proptics] def apply(indexed: Indexed[* => *, I, C, D]): S => T =
      self.over(other.over(indexed.runIndex))
  }

  /** compose a [[Setter_]] with an [[IndexedTraversal_]] */
  final def compose[I, C, D](other: IndexedTraversal_[I, A, B, C, D]): IndexedSetter_[I, S, T, C, D] = new IndexedSetter_[I, S, T, C, D] {
    override private[proptics] def apply(indexed: Indexed[* => *, I, C, D]): S => T =
      self(other(Indexed(indexed.runIndex)))
  }

  /** compose a [[Setter_]] with an [[IndexedTraversal_]] */
  final def compose[I, C, D](other: IndexedSetter_[I, A, B, C, D]): IndexedSetter_[I, S, T, C, D] = new IndexedSetter_[I, S, T, C, D] {
    override private[proptics] def apply(indexed: Indexed[* => *, I, C, D]): S => T =
      self(other(Indexed(indexed.runIndex)))
  }
}

object Setter_ {
  /** create a polymorphic setter from a mapping function */
  final def apply[S, T, A, B](mapping: (A => B) => S => T): Setter_[S, T, A, B] = new Setter_[S, T, A, B] {
    override def apply(pab: A => B): S => T = mapping(pab)
  }

  /** create a polymorphic setter from a [[cats.Functor]] */
  final def fromFunctor[F[_], A, B](implicit ev: Functor[F]): Setter_[F[A], F[B], A, B] = Setter_(ev.lift)

  /** create a polymorphic setter from a [[cats.Contravariant]] */
  final def fromContravariant[F[_], A, B](implicit ev: Contravariant[F]): Setter_[F[B], F[A], A, B] =
    Setter_(ev.liftContravariant)

  /** polymorphic identity of a [[Setter_]] */
  final def id[S, T]: Setter_[S, T, S, T] = Setter_[S, T, S, T](st => st)
}

object Setter {
  /** create a monomorphic setter from a mapping function */
  final def apply[S, A](mapping: (A => A) => S => S): Setter[S, A] = Setter_[S, S, A, A](mapping)

  /** create a monomorphic setter from a [[cats.Functor]] */
  final def fromFunctor[F[_], A](implicit ev: Functor[F]): Setter[F[A], A] = Setter_(ev.lift)

  /** create a monomorphic setter from a [[cats.Contravariant]] */
  final def fromContravariant[F[_], A](implicit ev: Contravariant[F]): Setter[F[A], A] =
    Setter_(ev.liftContravariant)

  /** monomorphic identity of a [[Setter]] */
  final def id[S]: Setter[S, S] = Setter_.id[S, S]
}
