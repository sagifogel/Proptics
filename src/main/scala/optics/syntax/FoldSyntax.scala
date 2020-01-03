package optics.syntax

import optics.Optic
import optics.internal.Forget
import optics.newtype.First

object FoldSyntax {
  implicit class FoldOps[R, S, T, A, B](val fold: Optic[Forget[R, *, *], S, T, A, B]) extends AnyVal {
    def foldMapOf(f: A => R)(s: S): R = fold(Forget(f)).runForget(s)
  }

  implicit class FoldFirstOps[S, T, A, B](val fold: Optic[Forget[First[A], *, * ], S, T, A, B]) extends AnyVal {
    def preview(s: S): Option[A] = fold.foldMapOf(First[A] _ compose Some[A])(s).runFirst
  }
}
