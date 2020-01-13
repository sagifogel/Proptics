package proptics.syntax

import cats.mtl.MonadState
import proptics.Optic
import proptics.internal.Forget
import proptics.syntax.OpticOnSyntax._

object GetterSyntax {
  implicit class GetterOps[R, S, T, A, B](val aGetter: Optic[Forget[A, *, *], S, T, A, B]) extends AnyVal {
    def view(s: S): A = aGetter(Forget(identity)).runForget(s)

    def use[M[_]](implicit ev: MonadState[M, S]): M[A] = ev.inspect(_ `^.` aGetter)
  }
}
