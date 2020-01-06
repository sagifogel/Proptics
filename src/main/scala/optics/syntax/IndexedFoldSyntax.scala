package optics.syntax

import cats.mtl.MonadState
import optics.IndexedOptic
import optics.internal.{Forget, Indexed}
import optics.newtype.Endo

import scala.Function.uncurried

object IndexedFoldSyntax {
  implicit class IndexedFoldOps[R, I, S, T, A, B](val indexedFold: IndexedOptic[Forget[R, *, *], I, S, T, A, B]) extends AnyVal {
    def foldMapOf(f: I => A => R)(s: S): R = indexedFold(Indexed(Forget(uncurried(f).tupled))).runForget(s)
  }

  implicit class IndexedFoldTupledOps[R, I, S, T, A, B](val indexedFold: IndexedOptic[Forget[(I, A), *, *], I, S, T, A, B]) extends AnyVal {
    def view(s: S): (I, A) = indexedFold(Indexed(Forget(identity))).runForget(s)

    def use[M[_]](implicit ev: MonadState[M, S]): M[(I, A)] = ev.inspect(indexedFold.view)

  }

  implicit class IndexedFoldEndoOps[R, I, S, T, A, B](val indexedFold: IndexedOptic[Forget[Endo[* => *, R], *, *], I, S, T, A, B]) extends AnyVal {
    def foldOf(f: I => A => R => R)(r: R)(s: S): R =
      indexedFold.foldMapOf(i => Endo[* => *, R] _ compose f(i))(s).runEndo(r)
  }
}
