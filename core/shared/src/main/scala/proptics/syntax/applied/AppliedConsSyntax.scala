package proptics.syntax.applied

import proptics._
import proptics.applied.{AppliedAffineTraversal, AppliedPrism}
import proptics.typeclass.Cons

trait AppliedConsSyntax {
  implicit final def consOps[S, A, B](s: S): ConsOps[S, A, B] = ConsOps(s)

  implicit final def appliedLensConsOps[S, A, B](appliedLens: AppliedLens[S, A]): AppliedLensConsOps[S, A, B] =
    AppliedLensConsOps(appliedLens)

  implicit final def appliedFoldConsOps[S, A, B](appliedFold: AppliedFold[S, A]): AppliedFoldConsOps[S, A, B] =
    AppliedFoldConsOps(appliedFold)

  implicit final def appliedAffineTraversalConsOps[S, A, B](appliedAffineTraversal: AppliedAffineTraversal[S, A]): AppliedAffineTraversalConsOps[S, A, B] =
    AppliedAffineTraversalConsOps(appliedAffineTraversal)

  implicit final def appliedTraversalConsOps[S, A, B](appliedTraversal: AppliedTraversal[S, A]): AppliedTraversalConsOps[S, A, B] =
    AppliedTraversalConsOps(appliedTraversal)
}

case class ConsOps[S, A, B](private val s: S) extends AnyVal {
  /** optionally splits the head and the tail of a data structure */
  def cons(implicit ev: Cons[S, A]): AppliedPrism[S, (A, S)] = AppliedPrism[S, (A, S)](s, ev.cons)

  /** optionally selects the first element of a data structure */
  def headOption(implicit ev: Cons[S, A]): AppliedAffineTraversal[S, A] = AppliedAffineTraversal(s, ev.headOption)

  /** optionally selects the tail of a data structure */
  def tailOption(implicit ev: Cons[S, A]): AppliedAffineTraversal[S, S] = AppliedAffineTraversal(s, ev.tailOption)
}

case class AppliedLensConsOps[S, A, B](private val appliedLens: AppliedLens[S, A]) extends AnyVal {
  /** optionally splits the head and the tail of a data structure */
  def cons(implicit ev: Cons[A, B]): AppliedAffineTraversal[S, (B, A)] = appliedLens.andThen(ev.cons)

  /** optionally selects the first element of a data structure */
  def headOption(implicit ev: Cons[A, B]): AppliedAffineTraversal[S, B] = appliedLens.andThen(ev.headOption)

  /** optionally selects the tail of a data structure */
  def tailOption(implicit ev: Cons[A, B]): AppliedAffineTraversal[S, A] = appliedLens.andThen(ev.tailOption)
}

case class AppliedFoldConsOps[S, A, B](private val appliedFold: AppliedFold[S, A]) extends AnyVal {
  /** optionally splits the head and the tail of a data structure */
  def cons(implicit ev: Cons[A, B]): AppliedFold[S, (B, A)] = appliedFold.andThen(ev.cons)

  /** optionally selects the first element of a data structure */
  def headOption(implicit ev: Cons[A, B]): AppliedFold[S, B] = appliedFold.andThen(ev.headOption)

  /** optionally selects the tail of a data structure */
  def tailOption(implicit ev: Cons[A, B]): AppliedFold[S, A] = appliedFold.andThen(ev.tailOption)
}

case class AppliedAffineTraversalConsOps[S, A, B](private val appliedAffineTraversal: AppliedAffineTraversal[S, A]) extends AnyVal {
  /** optionally splits the head and the tail of a data structure */
  def cons(implicit ev: Cons[A, B]): AppliedAffineTraversal[S, (B, A)] = appliedAffineTraversal.andThen(ev.cons)

  /** optionally selects the first element of a data structure */
  def headOption(implicit ev: Cons[A, B]): AppliedAffineTraversal[S, B] = appliedAffineTraversal.andThen(ev.headOption)

  /** optionally selects the tail of a data structure */
  def tailOption(implicit ev: Cons[A, B]): AppliedAffineTraversal[S, A] = appliedAffineTraversal.andThen(ev.tailOption)
}

case class AppliedTraversalConsOps[S, A, B](private val appliedTraversal: AppliedTraversal[S, A]) extends AnyVal {
  /** optionally splits the head and the tail of a data structure */
  def cons(implicit ev: Cons[A, B]): AppliedTraversal[S, (B, A)] = appliedTraversal.andThen(ev.cons)

  /** optionally selects the first element of a data structure */
  def headOption(implicit ev: Cons[A, B]): AppliedTraversal[S, B] = appliedTraversal.andThen(ev.headOption)

  /** optionally selects the tail of a data structure */
  def tailOption(implicit ev: Cons[A, B]): AppliedTraversal[S, A] = appliedTraversal.andThen(ev.tailOption)
}
