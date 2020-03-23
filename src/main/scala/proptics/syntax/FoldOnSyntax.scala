package proptics.syntax

import cats.Monoid
import proptics.Fold

import scala.reflect.ClassTag

object FoldOnSyntax {
  implicit class OpticOnOps[S](val s: S) extends AnyVal {
    def viewOn[T, A, B](aGetter: Fold[S, T, A, B])(implicit ev: Monoid[A]): List[A] = aGetter.viewAll(s)

    def `^.`[T, A, B](aGetter: Fold[S, T, A, B])(implicit ev: Monoid[A]): List[A] = viewOn(aGetter)

    def previewOn[T, A, B](fold: Fold[S, T, A, B]): Option[A] = fold.preview(s)

    def toListOn[T, A, B](fold: Fold[S, T, A, B])(implicit ev: Monoid[A]): List[A] = fold.toList(s)

    def `^..`[T, A, B](fold: Fold[S, T, A, B])(implicit ev: Monoid[A]): List[A] = toListOn(fold)

    def toArrayOn[T, A: ClassTag, B](fold: Fold[S, T, A, B])(implicit ev: Monoid[A]): Array[A] = fold.toArray(s)
  }
}
