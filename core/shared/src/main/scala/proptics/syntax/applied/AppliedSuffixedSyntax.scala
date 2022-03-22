package proptics.syntax.applied

import proptics.applied.{AppliedAffineTraversal, AppliedFold, AppliedPrism, AppliedTraversal}
import proptics.typeclass.Suffixed
import proptics.{AppliedAffineTraversal, AppliedFold, AppliedLens, AppliedPrism, AppliedTraversal}

trait AppliedSuffixedSyntax {
  implicit final def suffixedStringOps(s: String): SuffixedStringOps = SuffixedStringOps(s)

  implicit final def suffixedFaOps[F[_], G[_], A](fa: F[A]): SuffixedFaOps[F, G, A] = SuffixedFaOps(fa)

  implicit final def appliedLensSuffixedOps[S, A, B](appliedLens: AppliedLens[S, A]): AppliedLensSuffixedOps[S, A, B] =
    AppliedLensSuffixedOps(appliedLens)

  implicit final def appliedFoldSuffixedOps[S, A, B](appliedFold: AppliedFold[S, A]): AppliedFoldSuffixedOps[S, A, B] =
    AppliedFoldSuffixedOps(appliedFold)

  implicit final def appliedPrismSuffixedOps[S, A, B](appliedPrism: AppliedPrism[S, A]): AppliedPrismSuffixedOps[S, A, B] =
    AppliedPrismSuffixedOps(appliedPrism)

  implicit final def appliedAffineTraversalSuffixedOps[S, A, B](appliedAffineTraversal: AppliedAffineTraversal[S, A]): AppliedAffineTraversalSuffixedOps[S, A, B] =
    AppliedAffineTraversalSuffixedOps(appliedAffineTraversal)

  implicit final def appliedTraversalSuffixedOps[S, A, B](appliedTraversal: AppliedTraversal[S, A]): AppliedTraversalSuffixedOps[S, A, B] =
    AppliedTraversalSuffixedOps(appliedTraversal)
}

final case class SuffixedStringOps(private val s: String) extends AnyVal {
  /** stripping a suffix from a string */
  def suffixed(implicit ev: Suffixed[String, String]): AppliedPrism[String, String] = AppliedPrism(s, ev.suffixed(s))
}

final case class SuffixedFaOps[F[_], G[_], A](private val fa: F[A]) extends AnyVal {
  /** stripping a suffix from a data source `F[A]` */
  def suffixed(suffix: F[A])(implicit ev: Suffixed[F[A], G[A]]): AppliedPrism[F[A], G[A]] = AppliedPrism(fa, ev.suffixed(suffix))
}

final case class AppliedLensSuffixedOps[S, A, B](private val appliedLens: AppliedLens[S, A]) extends AnyVal {
  /** stripping a suffix from a data source `S` */
  def suffixed(suffix: A)(implicit ev: Suffixed[A, B]): AppliedAffineTraversal[S, B] =
    AppliedAffineTraversal(appliedLens.value, appliedLens.optic.andThen(ev.suffixed(suffix)))
}

final case class AppliedFoldSuffixedOps[S, A, B](private val appliedFold: AppliedFold[S, A]) extends AnyVal {
  /** stripping a suffix from a data source `S` */
  def suffixed(suffix: A)(implicit ev: Suffixed[A, B]): AppliedFold[S, B] =
    AppliedFold(appliedFold.value, appliedFold.optic.andThen(ev.suffixed(suffix)))
}

final case class AppliedPrismSuffixedOps[S, A, B](private val appliedPrism: AppliedPrism[S, A]) extends AnyVal {
  /** stripping a suffix from a data source `S` */
  def suffixed(suffix: A)(implicit ev: Suffixed[A, B]): AppliedPrism[S, B] =
    AppliedPrism(appliedPrism.value, appliedPrism.optic.andThen(ev.suffixed(suffix)))
}

final case class AppliedAffineTraversalSuffixedOps[S, A, B](private val appliedAffineTraversal: AppliedAffineTraversal[S, A]) extends AnyVal {
  /** stripping a suffix from a data source `S` */
  def suffixed(suffix: A)(implicit ev: Suffixed[A, B]): AppliedAffineTraversal[S, B] =
    AppliedAffineTraversal(appliedAffineTraversal.value, appliedAffineTraversal.optic.andThen(ev.suffixed(suffix)))
}

final case class AppliedTraversalSuffixedOps[S, A, B](private val appliedTraversal: AppliedTraversal[S, A]) extends AnyVal {
  /** stripping a suffix from a data source `S` */
  def suffixed(suffix: A)(implicit ev: Suffixed[A, B]): AppliedTraversal[S, B] =
    AppliedTraversal(appliedTraversal.value, appliedTraversal.optic.andThen(ev.suffixed(suffix)))
}
