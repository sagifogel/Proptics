package optics.syntax

import optics.Optic
import optics.internal.Forget
import optics.newtype.{Endo, First}
import optics.syntax.GetterSyntax._
import optics.syntax.FoldSyntax._

import scala.reflect.ClassTag

object OpticOnSyntax {
  implicit class OpticOnOps[S](val s: S) extends AnyVal {
    def viewOn[T, A, B](aGetter: Optic[Forget[A, *, *], S, T, A, B]): A = aGetter.view(s)

    def `^.`[T, A, B](aGetter: Optic[Forget[A, *, *], S, T, A, B]): A = viewOn(aGetter)

    def previewOn[T, A, B](fold: Optic[Forget[First[A], *, * ], S, T, A, B]): Option[A] = fold.preview(s)

    def toListOfOn[T, A, B](fold: Optic[Forget[Endo[* => *, List[A]], *, *], S, T, A, B]): List[A] = fold.toListOf(s)

    def `^..`[T, A, B](fold: Optic[Forget[Endo[* => *, List[A]], *, *], S, T, A, B]): List[A] = toListOfOn(fold)

    def toArrayOfOn[T, A: ClassTag, B](fold: Optic[Forget[Endo[* => *, List[A]], *, *], S, T, A, B]): Array[A] = fold.toArrayOf(s)
  }
}
