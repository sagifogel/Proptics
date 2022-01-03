package proptics.syntax.applied

import proptics.applied.AppliedIso
import proptics.typeclass.Reverse
import proptics.{AppliedAffineTraversal, AppliedFold, AppliedIso, AppliedLens, AppliedTraversal}

trait AppliedReverseSyntax {
  implicit final def appliedStringReverseOps(s: String): AppliedStringReverseOps = AppliedStringReverseOps(s)

  implicit final def appliedTuple2ReverseOps[A, B](tuple: (A, B)): AppliedTuple2ReverseOps[A, B] = AppliedTuple2ReverseOps(tuple)

  implicit final def appliedStringReverseOps[A, B, C](tuple: (A, B, C)): AppliedTuple3ReverseOps[A, B, C] = AppliedTuple3ReverseOps(tuple)

  implicit final def appliedCollectionReverseOps[F[_], A](fa: F[A]): AppliedCollectionReverseOps[F, A] = AppliedCollectionReverseOps(fa)

  implicit final def appliedLensReverseOps[S, A](appliedLens: AppliedLens[S, A]): AppliedLensReverseOps[S, A] =
    AppliedLensReverseOps(appliedLens)

  implicit final def appliedFoldReverseOps[S, A](appliedFold: AppliedFold[S, A]): AppliedFoldReverseOps[S, A] =
    AppliedFoldReverseOps(appliedFold)

  implicit final def appliedAffineTraversalReverseOps[S, A](appliedAffineTraversal: AppliedAffineTraversal[S, A]): AppliedAffineTraversalReverseOps[S, A] =
    AppliedAffineTraversalReverseOps(appliedAffineTraversal)

  implicit final def appliedTraversalReverseOps[S, A](appliedTraversal: AppliedTraversal[S, A]): AppliedTraversalReverseOps[S, A] =
    AppliedTraversalReverseOps(appliedTraversal)
}

case class AppliedStringReverseOps(private val s: String) extends AnyVal {
  /** reverse a String */
  def reverse(implicit ev: Reverse[String, String]): AppliedIso[String, String] = AppliedIso(s, ev.reverse)
}

case class AppliedTuple2ReverseOps[A, B](private val tuple: (A, B)) extends AnyVal {
  /** reverse a Tuple2 */
  def reverse(implicit ev: Reverse[(A, B), (B, A)]): AppliedIso[(A, B), (B, A)] = AppliedIso(tuple, ev.reverse)
}

case class AppliedTuple3ReverseOps[A, B, C](private val tuple: (A, B, C)) extends AnyVal {
  /** reverse a Tuple3 */
  def reverse(implicit ev: Reverse[(A, B, C), (C, B, A)]): AppliedIso[(A, B, C), (C, B, A)] = AppliedIso(tuple, ev.reverse)
}

case class AppliedCollectionReverseOps[F[_], A](private val fa: F[A]) extends AnyVal {
  /** reverse a data structure of F[A] */
  def reverse(implicit ev: Reverse[F[A], F[A]]): AppliedIso[F[A], F[A]] = AppliedIso(fa, ev.reverse)
}

case class AppliedLensReverseOps[S, A](private val appliedLens: AppliedLens[S, A]) extends AnyVal {
  /** reverse a data structure of F[A] */
  def reverse(implicit ev: Reverse[A, A]): AppliedLens[S, A] = appliedLens.andThen(ev.reverse)
}

case class AppliedFoldReverseOps[S, A](private val appliedFold: AppliedFold[S, A]) extends AnyVal {
  /** reverse a data structure of F[A] */
  def reverse(implicit ev: Reverse[A, A]): AppliedFold[S, A] = appliedFold.andThen(ev.reverse)
}

case class AppliedAffineTraversalReverseOps[S, A](private val appliedAffineTraversal: AppliedAffineTraversal[S, A]) extends AnyVal {
  /** reverse a structure of F[A] */
  def reverse(implicit ev: Reverse[A, A]): AppliedAffineTraversal[S, A] = appliedAffineTraversal.andThen(ev.reverse)
}

case class AppliedTraversalReverseOps[S, A](private val appliedTraversal: AppliedTraversal[S, A]) extends AnyVal {
  /** reverse a structure of F[A] */
  def reverse(implicit ev: Reverse[A, A]): AppliedTraversal[S, A] = appliedTraversal.andThen(ev.reverse)
}
