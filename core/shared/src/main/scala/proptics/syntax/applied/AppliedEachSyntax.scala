package proptics.syntax.applied

import cats.Traverse

import proptics._
import proptics.applied._
import proptics.typeclass.Each

trait AppliedEachSyntax {
  implicit final def eachOps[S, T, A, B](s: S): EachOps[S, T, A, B] = EachOps(s)

  implicit final def stringEachOps[S, T, A, B](s: String): StringEachOps = StringEachOps(s)

  implicit final def collectionEachOps[F[_], A](fa: F[A]): CollectionEachOps[F, A] = CollectionEachOps(fa)

  implicit final def appliedLensEachOps[S, T, F[_], A](appliedLens: AppliedLens_[S, T, F[A], F[A]]): AppliedLensEachOps[S, T, F, A] =
    AppliedLensEachOps[S, T, F, A](appliedLens)

  implicit final def appliedFoldEachOps[S, T, F[_], A](appliedFold: AppliedFold_[S, T, F[A], F[A]]): AppliedFoldEachOps[S, T, F, A] =
    AppliedFoldEachOps[S, T, F, A](appliedFold)

  implicit final def appliedPrismEachOps[S, T, F[_], A](appliedPrism: AppliedPrism_[S, T, F[A], F[A]]): AppliedPrismEachOps[S, T, F, A] =
    AppliedPrismEachOps[S, T, F, A](appliedPrism)

  implicit final def appliedAffineTraversalEachOps[S, T, F[_], A](appliedAffineTraversal: AppliedAffineTraversal_[S, T, F[A], F[A]]): AppliedAffineTraversalEachOps[S, T, F, A] =
    AppliedAffineTraversalEachOps[S, T, F, A](appliedAffineTraversal)

  implicit final def appliedTraversalEachOps[S, T, F[_], A](appliedTraversal: AppliedTraversal_[S, T, F[A], F[A]]): AppliedTraversalEachOps[S, T, F, A] =
    AppliedTraversalEachOps[S, T, F, A](appliedTraversal)
}

final case class StringEachOps(private val str: String) extends AnyVal {
  /** traverse each character of a String */
  def each(implicit ev: Each[String, Char]): AppliedTraversal[String, Char] =
    AppliedTraversal.apply[String, Char](str, ev.each)

  /** traverse each item `A` of a String using a [[Traversal_]] */
  def each[A](traversal: Traversal[String, A]): AppliedTraversal[String, A] =
    AppliedTraversal.apply[String, A](str, traversal)
}

final case class CollectionEachOps[F[_], A](private val fa: F[A]) extends AnyVal {
  /** traverse each item of a data structure `F[A]` */
  def each(implicit ev: Each[F[A], A]): AppliedTraversal[F[A], A] = AppliedTraversal(fa, ev.each)
}

final case class EachOps[S, T, A, B](private val s: S) extends AnyVal {
  /** traverse each item of a data structure using an [[Iso_]] */
  def eachT(iso: Iso_[S, T, A, B]): AppliedTraversal_[S, T, A, B] =
    AppliedTraversal_(s, Traversal_.id[S, T].andThen(iso))

  /** traverse each item of a data structure using an [[AnIso_]] */
  def eachT(iso: AnIso_[S, T, A, B]): AppliedTraversal_[S, T, A, B] =
    AppliedTraversal_(s, Traversal_.id[S, T].andThen(iso))

  /** traverse each item of a data structure using a [[Lens_]] */
  def eachT(lens: Lens_[S, T, A, B]): AppliedTraversal_[S, T, A, B] =
    AppliedTraversal_(s, Traversal_.id[S, T].andThen(lens))

  /** traverse each item of a data structure using an [[ALens_]] */
  def eachT(aLens: ALens_[S, T, A, B]): AppliedTraversal_[S, T, A, B] =
    AppliedTraversal_(s, Traversal_.id[S, T].andThen(aLens))

  /** traverse each item of a data structure using a [[Prism]] */
  def eachT(prism: Prism_[S, T, A, B]): AppliedTraversal_[S, T, A, B] =
    AppliedTraversal_(s, Traversal_.id[S, T].andThen(prism))

  /** traverse each item of a data structure using an [[APrism_]] */
  def eachT(aPrism: APrism_[S, T, A, B]): AppliedTraversal_[S, T, A, B] =
    AppliedTraversal_(s, Traversal_.id[S, T].andThen(aPrism))

  /** traverse each item of a data structure using an [[AffineTraversal_]] */
  def eachT(affineTraversal: AffineTraversal_[S, T, A, B]): AppliedTraversal_[S, T, A, B] =
    AppliedTraversal_(s, Traversal_.id[S, T].andThen(affineTraversal))

  /** traverse each item of a data structure using an [[AnAffineTraversal_]] */
  def eachT(anAffineTraversal: AnAffineTraversal_[S, T, A, B]): AppliedTraversal_[S, T, A, B] =
    AppliedTraversal_(s, Traversal_.id[S, T].andThen(anAffineTraversal))

  /** traverse each item of a data structure using an [[ATraversal_]] */
  def eachT(traversal: Traversal_[S, T, A, B]): AppliedTraversal_[S, T, A, B] =
    AppliedTraversal_(s, traversal)

  /** traverse each item of a data structure using a [[Traversal_]] */
  def eachT(aTraversal: ATraversal_[S, T, A, B]): AppliedTraversal_[S, T, A, B] =
    AppliedTraversal_(s, Traversal_.id[S, T].andThen(aTraversal))
}

final case class AppliedLensEachOps[S, T, F[_], A](private val appliedLens: AppliedLens_[S, T, F[A], F[A]]) extends AnyVal {
  /** traverse each item of a data structure `F[A]` */
  def each(implicit ev0: Traverse[F]): AppliedTraversal_[S, T, A, A] =
    AppliedTraversal_(appliedLens.value, appliedLens.optic.andThen(Traversal.fromTraverse[F, A]))
}

final case class AppliedFoldEachOps[S, T, F[_], A](private val appliedFold: AppliedFold_[S, T, F[A], F[A]]) extends AnyVal {
  /** traverse each item of a data structure `F[A]` */
  def each(implicit ev0: Traverse[F]): AppliedFold_[S, T, A, A] =
    AppliedFold_(appliedFold.value, appliedFold.optic.andThen(Traversal.fromTraverse[F, A]))
}

final case class AppliedPrismEachOps[S, T, F[_], A](private val appliedPrism: AppliedPrism_[S, T, F[A], F[A]]) extends AnyVal {
  /** traverse each item of a data structure `F[A]` */
  def each(implicit ev0: Traverse[F]): AppliedTraversal_[S, T, A, A] =
    AppliedTraversal_(appliedPrism.value, appliedPrism.optic.andThen(Traversal.fromTraverse[F, A]))
}

final case class AppliedAffineTraversalEachOps[S, T, F[_], A](private val appliedAffineTraversal: AppliedAffineTraversal_[S, T, F[A], F[A]]) extends AnyVal {
  /** traverse each item of a data structure `F[A]` */
  def each(implicit ev0: Traverse[F]): AppliedTraversal_[S, T, A, A] =
    AppliedTraversal_(appliedAffineTraversal.value, appliedAffineTraversal.optic.andThen(Traversal.fromTraverse[F, A]))
}

final case class AppliedTraversalEachOps[S, T, F[_], A](private val appliedTraversal: AppliedTraversal_[S, T, F[A], F[A]]) extends AnyVal {
  /** traverse each item of a data structure `F[A]` */
  def each(implicit ev0: Traverse[F]): AppliedTraversal_[S, T, A, A] =
    AppliedTraversal_(appliedTraversal.value, appliedTraversal.optic.andThen(Traversal.fromTraverse[F, A]))
}
