package proptics.syntax

import proptics._
import proptics.applied.AppliedAffineTraversal
import proptics.typeclass.Index

trait IndexSyntax {
  implicit final def appliedIndexOfCollectionOps[F[_], A](fa: F[A]): AppliedIndexOfCollectionOps[F, A] = AppliedIndexOfCollectionOps(fa)

  implicit final def lensIndexOps[S, T, A](lens: Lens[S, T]): LensIndexOps[S, T, A] = LensIndexOps(lens)

  implicit final def traversalIndexOps[S, T, A](traversal: Traversal[S, T]): TraversalIndexOps[S, T, A] = TraversalIndexOps(traversal)
}

final case class AppliedIndexOfCollectionOps[F[_], A](private val fa: F[A]) extends AnyVal {
  /** traverse a value at a given index of a data structure `F[A]` */
  def index[I](i: I)(implicit ev: Index[F[A], I, A]): AppliedAffineTraversal[F[A], A] = AppliedAffineTraversal(fa, ev.ix(i))
}

final case class AppliedPrismIndexOps[S, T, A](private val appliedPrism: AppliedPrism[S, T]) extends AnyVal {
  /** traverse a value at a given index of a data structure `S` */
  def index[I](i: I)(implicit ev: Index[T, I, A]): AppliedAffineTraversal[S, A] = appliedPrism.andThen(ev.ix(i))
}

final case class LensIndexOps[S, T, A](private val lens: Lens[S, T]) extends AnyVal {
  /** traverse a value at a given index of a data structure `S` */
  def index[I](i: I)(implicit ev: Index[T, I, A]): AffineTraversal[S, A] = lens.andThen(ev.ix(i))
}

final case class TraversalIndexOps[S, T, A](private val traversal: Traversal[S, T]) extends AnyVal {
  /** traverse a value at a given index of a data structure `S` */
  def index[I](i: I)(implicit ev: Index[T, I, A]): Traversal[S, A] = traversal.andThen(ev.ix(i))
}
