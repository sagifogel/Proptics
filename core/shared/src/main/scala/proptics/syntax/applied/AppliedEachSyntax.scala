package proptics.syntax.applied

import cats.Traverse

import proptics.applied.{AppliedTraversal, AppliedTraversal_}
import proptics.typeclass.Each
import proptics.{AppliedTraversal, Iso_, Traversal, Traversal_}

trait AppliedEachSyntax {
  implicit final def eachOps[S, T, A, B](s: S): EachOps[S, T, A, B] = EachOps(s)

  implicit final def eachAppliedTraversalOps[S, T, F[_], A](appliedTraversal: AppliedTraversal_[S, T, F[A], F[A]]): EachAppliedTraversalOps[S, T, F, A] =
    EachAppliedTraversalOps[S, T, F, A](appliedTraversal)
}

case class EachStringOps(private val str: String) extends AnyVal {
  /** traverse each character of a String */
  def each(implicit ev: Each[String, Char]): AppliedTraversal[String, Char] =
    AppliedTraversal.apply[String, Char](str, ev.each)

  /** traverse each item `A` of a String using a [[Traversal_]] */
  def each[A](traversal: Traversal[String, A]): AppliedTraversal[String, A] =
    AppliedTraversal.apply[String, A](str, traversal)
}

case class EachOps[S, T, A, B](private val s: S) extends AnyVal {
  /** traverse each item of a container `S` */
  def each(implicit ev: Each[S, A]): AppliedTraversal[S, A] =
    AppliedTraversal.apply[S, A](s, ev.each)

  /** traverse each item of a container using a [[Traversal_]] */
  def eachT(traversal: Traversal_[S, T, A, B]): AppliedTraversal_[S, T, A, B] =
    AppliedTraversal_(s, traversal)

  /** traverse each item of a container using an [[Iso_]] */
  def eachT(iso: Iso_[S, T, A, B]): AppliedTraversal_[S, T, A, B] =
    AppliedTraversal_(s, Traversal_.id[S, T].andThen(iso))
}

case class EachAppliedTraversalOps[S, T, F[_], A](private val appliedTraversal: AppliedTraversal_[S, T, F[A], F[A]]) extends AnyVal {
  /** compose this [[Traversal_]] with a [[Traversal_]], having this [[Traversal_]] applied first */
  def each(implicit ev0: Traverse[F]): AppliedTraversal_[S, T, A, A] =
    AppliedTraversal_(appliedTraversal.value, appliedTraversal.optic.andThen(Traversal.fromTraverse[F, A]))
}
