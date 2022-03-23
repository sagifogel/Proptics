package proptics.syntax.applied

import proptics._
import proptics.applied.AppliedLens
import proptics.typeclass.At

trait AppliedAtSyntax {
  implicit final def appliedAtSetOps[A](fa: Set[A]): AppliedAtOfSetOps[A] = AppliedAtOfSetOps(fa)

  implicit final def appliedAtOfMapLikeOps[M[_, _], K, V](fa: M[K, V]): AppliedAtOfMapLikeOps[M, K, V] = AppliedAtOfMapLikeOps(fa)

  implicit final def appliedLensAtOps[S, T, A](appliedLens: AppliedLens[S, T]): AppliedLensAtOps[S, T, A] = AppliedLensAtOps(appliedLens)

  implicit final def appliedFoldAtOps[S, T, A](appliedFold: AppliedFold[S, T]): AppliedFoldAtOps[S, T, A] = AppliedFoldAtOps(appliedFold)

  implicit final def appliedPrismAtOps[S, T, A](appliedPrism: AppliedPrism[S, T]): AppliedPrismAtOps[S, T, A] = AppliedPrismAtOps(appliedPrism)

  implicit final def appliedAffineTraversalAtOps[S, T, A](appliedAffineTraversal: AppliedAffineTraversal[S, T]): AppliedAffineTraversalAtOps[S, T, A] = AppliedAffineTraversalAtOps(
    appliedAffineTraversal)

  implicit final def appliedTraversalAtOps[S, T, A](appliedTraversal: AppliedTraversal[S, T]): AppliedTraversalAtOps[S, T, A] = AppliedTraversalAtOps(appliedTraversal)
}

final case class AppliedAtOfSetOps[A](private val set: Set[A]) extends AnyVal {
  /** traverse a value at a given key of a `Set[A]` */
  def at(k: A)(implicit ev: At[Set[A], A, Unit]): AppliedLens[Set[A], Option[Unit]] = AppliedLens(set, ev.at(k))
}

final case class AppliedAtOfMapLikeOps[M[_, _], K, V](private val map: M[K, V]) extends AnyVal {
  /** traverse a value at a given key of a map like data structure of `M[K, V]` */
  def at(k: K)(implicit ev: At[M[K, V], K, V]): AppliedLens[M[K, V], Option[V]] = AppliedLens(map, ev.at(k))
}

final case class AppliedLensAtOps[S, T, A](private val appliedLens: AppliedLens[S, T]) extends AnyVal {
  /** traverse a value at a given key of a data structure `S` */
  def at[I](i: I)(implicit ev: At[T, I, A]): AppliedLens[S, Option[A]] = appliedLens.andThen(ev.at(i))
}

final case class AppliedFoldAtOps[S, T, A](private val appliedFold: AppliedFold[S, T]) extends AnyVal {
  /** traverse a value at a given key of a data structure `S` */
  def at[I](i: I)(implicit ev: At[T, I, A]): AppliedFold[S, Option[A]] = appliedFold.andThen(ev.at(i))
}

final case class AppliedPrismAtOps[S, T, A](private val appliedPrism: AppliedPrism[S, T]) extends AnyVal {
  /** traverse a value at a given key of a data structure `S` */
  def at[I](i: I)(implicit ev: At[T, I, A]): AppliedAffineTraversal[S, Option[A]] = appliedPrism.andThen(ev.at(i))
}

final case class AppliedAffineTraversalAtOps[S, T, A](private val appliedAffineTraversal: AppliedAffineTraversal[S, T]) extends AnyVal {
  /** traverse a value at a given key of a data structure `S` */
  def at[I](i: I)(implicit ev: At[T, I, A]): AppliedFold[S, Option[A]] = appliedAffineTraversal.andThen(ev.at(i))
}

final case class AppliedTraversalAtOps[S, T, A](private val appliedTraversal: AppliedTraversal[S, T]) extends AnyVal {
  /** traverse a value at a given key of a data structure `S` */
  def at[I](i: I)(implicit ev: At[T, I, A]): AppliedFold[S, Option[A]] = appliedTraversal.andThen(ev.at(i))
}
