package proptics.syntax

import proptics.Fold
import proptics.newtype.{Endo, First}
import proptics.syntax.FoldSyntax._

import scala.reflect.ClassTag

object FoldOnSyntax {
  implicit class OpticOnOps[S](val s: S) extends AnyVal {
    def viewOn[T, A, B](aGetter: Fold[A, S, T, A, B]): A = aGetter.view(s)

    def `^.`[T, A, B](aGetter: Fold[A, S, T, A, B]): A = viewOn(aGetter)

    def preview[T, A, B](fold: Fold[First[A], S, T, A, B]): Option[A] = fold.preview(s)

    def toListOn[T, A, B](fold: Fold[Endo[* => *, List[A]], S, T, A, B]): List[A] = fold.toList(s)

    def `^..`[T, A, B](fold: Fold[Endo[* => *, List[A]], S, T, A, B]): List[A] = toListOn(fold)

    def toArrayOfOn[T, A: ClassTag, B](fold: Fold[Endo[* => *, List[A]], S, T, A, B]): Array[A] = fold.toArray(s)
  }
}
