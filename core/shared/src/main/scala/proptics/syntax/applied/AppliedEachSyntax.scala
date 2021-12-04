package proptics.syntax.applied

import cats.Traverse

import proptics.applied.{AppliedTraversal, AppliedTraversal_}
import proptics.typeclass.Each
import proptics.{AppliedTraversal, Iso_, Traversal, Traversal_}

trait AppliedEachSyntax {
  implicit final def eachOps[S, T, A, B](s: S): EachOps[S, T, A, B] = EachOps(s)

  implicit final def eachAppliedTraversalOps[F[_], A](appliedTraversal: AppliedTraversal[F[F[A]], F[A]]): EachAppliedTraversalOps[F, A] =
    EachAppliedTraversalOps[F, A](appliedTraversal)
}

case class EachStringOps(str: String) extends AnyVal {
  def each(implicit ev: Each[String, Char]): AppliedTraversal[String, Char] =
    AppliedTraversal.apply[String, Char](str, ev.each)

  def each[A](traversal: Traversal[String, A]): AppliedTraversal[String, A] =
    AppliedTraversal.apply[String, A](str, traversal)
}

case class EachOps[S, T, A, B](s: S) extends AnyVal {
  def each(implicit ev: Each[S, A]): AppliedTraversal[S, A] =
    AppliedTraversal.apply[S, A](s, ev.each)

  def eachT(traversal: Traversal_[S, T, A, B]): AppliedTraversal_[S, T, A, B] =
    AppliedTraversal_(s, traversal)

  def eachT(iso: Iso_[S, T, A, B]): AppliedTraversal_[S, T, A, B] =
    AppliedTraversal_(s, Traversal_.id[S, T].andThen(iso))
}

case class EachAppliedTraversalOps[F[_], A](appliedTraversal: AppliedTraversal[F[F[A]], F[A]]) extends AnyVal {
  def each(implicit ev0: Traverse[F]): AppliedTraversal[F[F[A]], A] =
    AppliedTraversal.apply(appliedTraversal.value, appliedTraversal.optic.andThen(Traversal.fromTraverse[F, A]))
}
