package optics.syntax

import cats.mtl.MonadState
import optics.Optic
import optics.internal.Forget

object GetterSyntax {
  implicit class GetterOps[R, S, T, A, B](val aGetter: Optic[Forget[A, *, *], S, T, A, B]) extends AnyVal {
    def view(s: S): A = aGetter(Forget(identity)).runForget(s)

    def use[M[_]](implicit ev: MonadState[M, S]): M[A] = ev.inspect(_ `^.` aGetter)
  }

  implicit class ViewGetterOps[S](val s: S) extends AnyVal {
    def viewOn[T, A, B](aGetter: Optic[Forget[A, *, *], S, T, A, B]): A = aGetter.view(s)

    def `^.`[T, A, B](aGetter: Optic[Forget[A, *, *], S, T, A, B]): A = viewOn(aGetter)
  }
}
