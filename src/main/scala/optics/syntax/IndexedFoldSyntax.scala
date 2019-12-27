package optics.syntax

import cats.mtl.MonadState
import optics.IndexedFold
import optics.internal.{Forget, Indexed}

object IndexedFoldSyntax {
  implicit class IndexedFoldTupledOps[R, I, S, T, A, B](val indexedFold: IndexedFold[(I, A), I, S, T, A, B]) extends AnyVal {
    def iview(s: S): (I, A) = indexedFold(Indexed(Forget(identity))).runForget(s)

    def iuse[M[_]](implicit ev: MonadState[M, S]): M[(I, A)] = ev.inspect(indexedFold.iview)
  }
}
