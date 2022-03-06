package proptics.syntax.applied

import proptics.applied.{AppliedAffineTraversal, AppliedFold, AppliedPrism, AppliedTraversal}
import proptics.typeclass.Prefixed
import proptics.{AppliedAffineTraversal, AppliedFold, AppliedLens, AppliedPrism, AppliedTraversal}

trait AppliedPrefixedSyntax {
  implicit final def prefixedStringOps(s: String): PrefixedStringOps = PrefixedStringOps(s)

  implicit final def prefixedFaOps[F[_], G[_], A](fa: F[A]): PrefixedFaOps[F, G, A] = PrefixedFaOps(fa)

  implicit final def appliedLensPrefixedOps[S, A, B](appliedLens: AppliedLens[S, A]): AppliedLensPrefixedOps[S, A, B] =
    AppliedLensPrefixedOps(appliedLens)

  implicit final def appliedFoldPrefixedOps[S, A, B](appliedFold: AppliedFold[S, A]): AppliedFoldPrefixedOps[S, A, B] =
    AppliedFoldPrefixedOps(appliedFold)

  implicit final def appliedAffineTraversalPrefixedOps[S, A, B](appliedAffineTraversal: AppliedAffineTraversal[S, A]): AppliedAffineTraversalPrefixedOps[S, A, B] =
    AppliedAffineTraversalPrefixedOps(appliedAffineTraversal)

  implicit final def appliedTraversalPrefixedOps[S, A, B](appliedTraversal: AppliedTraversal[S, A]): AppliedTraversalPrefixedOps[S, A, B] =
    AppliedTraversalPrefixedOps(appliedTraversal)
}

case class PrefixedStringOps(private val s: String) extends AnyVal {
  /** stripping a prefix from a string */
  def prefixed(implicit ev: Prefixed[String, String]): AppliedPrism[String, String] = AppliedPrism(s, ev.prefixed(s))
}

case class PrefixedFaOps[F[_], G[_], A](private val fa: F[A]) extends AnyVal {
  /** stripping a prefix from a data structure of `F[A]` */
  def prefixed(prefix: F[A])(implicit ev: Prefixed[F[A], G[A]]): AppliedPrism[F[A], G[A]] = AppliedPrism(fa, ev.prefixed(prefix))
}

case class AppliedLensPrefixedOps[S, A, B](private val appliedLens: AppliedLens[S, A]) extends AnyVal {
  /** stripping a prefix from a data structure of `S` */
  def prefixed(prefix: A)(implicit ev: Prefixed[A, B]): AppliedAffineTraversal[S, B] =
    AppliedAffineTraversal(appliedLens.value, appliedLens.optic.andThen(ev.prefixed(prefix)))
}

case class AppliedFoldPrefixedOps[S, A, B](private val appliedFold: AppliedFold[S, A]) extends AnyVal {
  /** stripping a prefix from a data structure of `S` */
  def prefixed(prefix: A)(implicit ev: Prefixed[A, B]): AppliedFold[S, B] =
    AppliedFold(appliedFold.value, appliedFold.optic.andThen(ev.prefixed(prefix)))
}

case class AppliedAffineTraversalPrefixedOps[S, A, B](private val appliedAffineTraversal: AppliedAffineTraversal[S, A]) extends AnyVal {
  /** stripping a prefix from a data structure of `S` */
  def prefixed(prefix: A)(implicit ev: Prefixed[A, B]): AppliedAffineTraversal[S, B] =
    AppliedAffineTraversal(appliedAffineTraversal.value, appliedAffineTraversal.optic.andThen(ev.prefixed(prefix)))
}

case class AppliedTraversalPrefixedOps[S, A, B](private val appliedTraversal: AppliedTraversal[S, A]) extends AnyVal {
  /** stripping a prefix from a data structure of `S` */
  def prefixed(prefix: A)(implicit ev: Prefixed[A, B]): AppliedTraversal[S, B] =
    AppliedTraversal(appliedTraversal.value, appliedTraversal.optic.andThen(ev.prefixed(prefix)))
}
