package proptics.syntax

import cats.Monoid
import proptics.{Fold, Fold_}

import scala.reflect.ClassTag

trait FoldSyntax {
  implicit def foldOnSyntax[S](s: S) = FoldOnSyntax(s)
}

final case class FoldOnSyntax[S](private val s: S) extends AnyVal {
  import proptics.syntax.fold._

  def viewOn_[T, A: Monoid, B](fold: Fold_[S, T, A, B]): A = fold.view(s)

  def viewOn[A: Monoid](fold: Fold[S, A]): A = fold.view(s)

  def viewAllOn_[T, A: Monoid, B](fold: Fold_[S, T, A, B]): List[A] = s.toListOn_(fold)

  def viewAllOn[A: Monoid](fold: Fold[S, A]): List[A] = s.toListOn(fold)

  def `^._`[T, A: Monoid, B](fold: Fold_[S, T, A, B]): A = viewOn_(fold)

  def `^.`[A: Monoid](fold: Fold[S, A]): A = viewOn_(fold)

  def previewOn_[T, A: Monoid, B](fold: Fold_[S, T, A, B]): Option[A] = fold.preview(s)

  def previewOn[A: Monoid](fold: Fold[S, A]): Option[A] = fold.preview(s)

  def toListOn_[T, A: Monoid, B](fold: Fold_[S, T, A, B]): List[A] = fold.toList(s)

  def toListOn[A: Monoid](fold: Fold[S, A]): List[A] = fold.toList(s)

  def `^.._`[T, A: Monoid, B](fold: Fold_[S, T, A, B]): List[A] = toListOn_(fold)

  def `^..`[A: Monoid](fold: Fold[S, A]): List[A] = toListOn(fold)

  def toArrayOn_[T, A: ClassTag: Monoid, B](fold: Fold_[S, T, A, B]): Array[A] = fold.toArray(s)

  def toArrayOn[A: ClassTag: Monoid](fold: Fold[S, A]): Array[A] = fold.toArray(s)
}
