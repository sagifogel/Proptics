package proptics.syntax.applied

import proptics._
import proptics.typeclass.Index

trait AppliedIndexSyntax {
  implicit final def appliedLensIndexOps[S, T, A](appliedLens: AppliedLens[S, T]): AppliedLensIndexOps[S, T, A] = AppliedLensIndexOps(appliedLens)

  implicit final def appliedLFoldIndexOps[S, T, A](appliedFold: AppliedFold[S, T]): AppliedFoldIndexOps[S, T, A] = AppliedFoldIndexOps(appliedFold)

  implicit final def appliedPrismIndexOps[S, T, A](appliedPrism: AppliedPrism[S, T]): AppliedPrismIndexOps[S, T, A] = AppliedPrismIndexOps(appliedPrism)

  implicit final def appliedAffineTraversalIndexOps[S, T, A](appliedAffineTraversal: AppliedAffineTraversal[S, T]): AppliedAffineTraversalIndexOps[S, T, A] =
    AppliedAffineTraversalIndexOps(appliedAffineTraversal)

  implicit final def appliedTraversalIndexOps[S, T, A](appliedTraversal: AppliedTraversal[S, T]): AppliedTraversalIndexOps[S, T, A] = AppliedTraversalIndexOps(appliedTraversal)
}

final case class AppliedLensIndexOps[S, T, A](private val appliedLens: AppliedLens[S, T]) extends AnyVal {
  /** traverse a value at a given index of a data structure `S` */
  def index[I](i: I)(implicit ev: Index[T, I, A]): AppliedAffineTraversal[S, A] = appliedLens.andThen(ev.ix(i))
}

final case class AppliedFoldIndexOps[S, T, A](private val appliedFold: AppliedFold[S, T]) extends AnyVal {
  /** traverse a value at a given index of a data structure `S` */
  def index[I](i: I)(implicit ev: Index[T, I, A]): AppliedFold[S, A] = appliedFold.andThen(ev.ix(i))
}

final case class AppliedPrismIndexOps[S, T, A](private val appliedPrism: AppliedPrism[S, T]) extends AnyVal {
  /** traverse a value at a given index of a data structure `S` */
  def index[I](i: I)(implicit ev: Index[T, I, A]): AppliedAffineTraversal[S, A] = appliedPrism.andThen(ev.ix(i))
}

final case class AppliedAffineTraversalIndexOps[S, T, A](private val appliedAffineTraversal: AppliedAffineTraversal[S, T]) extends AnyVal {
  /** traverse a value at a given index of a data structure `S` */
  def index[I](i: I)(implicit ev: Index[T, I, A]): AppliedAffineTraversal[S, A] = appliedAffineTraversal.andThen(ev.ix(i))
}

final case class AppliedTraversalIndexOps[S, T, A](private val appliedTraversal: AppliedTraversal[S, T]) extends AnyVal {
  /** traverse a value at a given index of a data structure `S` */
  def index[I](i: I)(implicit ev: Index[T, I, A]): AppliedFold[S, A] = appliedTraversal.andThen(ev.ix(i))
}
