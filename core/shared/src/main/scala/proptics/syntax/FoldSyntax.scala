package proptics.syntax

import cats.Monoid
import proptics.{Fold, Fold_}

import scala.reflect.ClassTag

trait FoldSyntax {
  implicit def foldOnSyntax[S](s: S) = FoldOnSyntax(s)
}

final case class FoldOnSyntax[S](private val s: S) extends AnyVal {
  import proptics.syntax.fold._

  def viewOn[A: Monoid](fold: Fold[S, A]): A = fold.view(s)

  def viewAllOn[A](fold: Fold[S, A]): List[A] = s.toListOn(fold)

  def `^.`[A: Monoid](fold: Fold[S, A]): A = viewOn(fold)

  def previewOn[A: Monoid](fold: Fold[S, A]): Option[A] = fold.preview(s)

  def toListOn[A](fold: Fold[S, A]): List[A] = fold.toList(s)

  def `^..`[A: Monoid](fold: Fold[S, A]): List[A] = toListOn(fold)

  def toArrayOn[T, A: ClassTag, B](fold: Fold_[S, T, A, B]): Array[A] = fold.toArray(s)
}
