package proptics.syntax.applied

import proptics._
import proptics.applied.{AppliedAffineTraversal, AppliedPrism}
import proptics.typeclass.Cons

trait AppliedConsSyntax {
  implicit final def consOps[S, T, A, B](s: S): ConsOps[S, T, A, B] = ConsOps(s)

  implicit final def appliedLensConsOps[S, T, A, B](appliedLens: AppliedLens[S, A]): AppliedLensConsOps[S, T, A, B] =
    AppliedLensConsOps(appliedLens)

  implicit final def appliedFoldConsOps[S, T, A, B](appliedFold: AppliedFold[S, A]): AppliedFoldConsOps[S, T, A, B] =
    AppliedFoldConsOps(appliedFold)

  implicit final def appliedTraversalConsOps[S, T, A, B](appliedTraversal: AppliedTraversal[S, A]): AppliedTraversalConsOps[S, T, A, B] =
    AppliedTraversalConsOps(appliedTraversal)
}

case class ConsOps[S, T, A, B](s: S) extends AnyVal {
  /** optionally splits the head and the tail of a data structure using [[Prism]] */
  def cons(implicit ev: Cons[S, A]): AppliedPrism[S, (A, S)] = AppliedPrism[S, (A, S)](s, ev.cons)

  /** optionally selects the first element of a data structure using a [[Prism]] */
  def headOption(implicit ev: Cons[S, A]): AppliedAffineTraversal[S, A] = AppliedAffineTraversal(s, ev.headOption)

  /** optionally selects the tail of a data structure using a [[Prism]] */
  def tailOption(implicit ev: Cons[S, A]): AppliedAffineTraversal[S, S] = AppliedAffineTraversal(s, ev.tailOption)
}

case class AppliedLensConsOps[S, T, A, B](appliedLens: AppliedLens[S, A]) extends AnyVal {
  /** optionally splits the head and the tail of a data structure using [[Prism]] */
  def cons(implicit ev: Cons[A, B]): AppliedAffineTraversal[S, (B, A)] = appliedLens.andThen(ev.cons)

  /** optionally selects the first element of a data structure using a [[Prism]] */
  def headOption(implicit ev: Cons[A, B]): AppliedAffineTraversal[S, B] = appliedLens.andThen(ev.headOption)

  /** optionally selects the tail of a data structure using a [[Prism]] */
  def tailOption(implicit ev: Cons[A, B]): AppliedAffineTraversal[S, A] = appliedLens.andThen(ev.tailOption)
}

case class AppliedFoldConsOps[S, T, A, B](appliedFold: AppliedFold[S, A]) extends AnyVal {
  /** optionally splits the head and the tail of a data structure using [[Prism]] */
  def cons(implicit ev: Cons[A, B]): AppliedFold[S, (B, A)] = appliedFold.andThen(ev.cons)

  /** optionally selects the first element of a data structure using a [[Prism]] */
  def headOption(implicit ev: Cons[A, B]): AppliedFold[S, B] = appliedFold.andThen(ev.headOption)

  /** optionally selects the tail of a data structure using a [[Prism]] */
  def tailOption(implicit ev: Cons[A, B]): AppliedFold[S, A] = appliedFold.andThen(ev.tailOption)
}

case class AppliedTraversalConsOps[S, T, A, B](appliedTraversal: AppliedTraversal[S, A]) extends AnyVal {
  /** optionally splits the head and the tail of a data structure using [[Prism]] */
  def cons(implicit ev: Cons[A, B]): AppliedTraversal[S, (B, A)] = appliedTraversal.andThen(ev.cons)

  /** optionally selects the first element of a data structure using a [[Prism]] */
  def headOption(implicit ev: Cons[A, B]): AppliedTraversal[S, B] = appliedTraversal.andThen(ev.headOption)

  /** optionally selects the tail of a data structure using a [[Prism]] */
  def tailOption(implicit ev: Cons[A, B]): AppliedTraversal[S, A] = appliedTraversal.andThen(ev.tailOption)
}
