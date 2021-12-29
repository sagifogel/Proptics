package proptics.syntax.applied

import proptics.applied.AppliedAffineTraversal
import proptics.typeclass.Index
import proptics.{AppliedAffineTraversal, AppliedFold, AppliedLens, AppliedTraversal}

trait AppliedIndexSyntax {
  implicit final def appliedIndexOfCollectionOps[F[_], A](fa: F[A]): AppliedIndexOfCollectionOps[F, A] = AppliedIndexOfCollectionOps(fa)

  implicit final def appliedLensIndexOps[S, T, A](appliedLens: AppliedLens[S, T]): AppliedLensIndexOps[S, T, A] = AppliedLensIndexOps(appliedLens)

  implicit final def appliedLFoldIndexOps[S, T, A](appliedFold: AppliedFold[S, T]): AppliedFoldIndexOps[S, T, A] = AppliedFoldIndexOps(appliedFold)

  implicit final def appliedTraversalIndexOps[S, T, A](appliedTraversal: AppliedTraversal[S, T]): AppliedTraversalIndexOps[S, T, A] = AppliedTraversalIndexOps(appliedTraversal)
}

case class AppliedIndexOfCollectionOps[F[_], A](private val fa: F[A]) extends AnyVal {
  /** traverse a value at a given key/index of a data structure `F[A]` */
  def index[I](i: I)(implicit ev: Index[F[A], I, A]): AppliedAffineTraversal[F[A], A] = AppliedAffineTraversal(fa, ev.ix(i))
}

case class AppliedLensIndexOps[S, T, A](private val appliedLens: AppliedLens[S, T]) extends AnyVal {
  /** traverse a value at a given key/index of a data structure `S` */
  def index[I](i: I)(implicit ev: Index[T, I, A]): AppliedAffineTraversal[S, A] = appliedLens.andThen(ev.ix(i))
}

case class AppliedFoldIndexOps[S, T, A](private val appliedFold: AppliedFold[S, T]) extends AnyVal {
  /** traverse a value at a given key/index of a data structure `S` */
  def index[I](i: I)(implicit ev: Index[T, I, A]): AppliedFold[S, A] = appliedFold.andThen(ev.ix(i))
}

case class AppliedTraversalIndexOps[S, T, A](private val appliedTraversal: AppliedTraversal[S, T]) extends AnyVal {
  /** traverse a value at a given key/index of a data structure `S` */
  def index[I](i: I)(implicit ev: Index[T, I, A]): AppliedFold[S, A] = appliedTraversal.andThen(ev.ix(i))
}
