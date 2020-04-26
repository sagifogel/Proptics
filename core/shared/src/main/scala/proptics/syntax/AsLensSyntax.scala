package proptics.syntax

import proptics.{AnIndexedLens, IndexedLens, Lens}

object AsLensSyntax {
  implicit class AnLensAsLensOps[I, S, A](val anIndexedLens: AnIndexedLens[I, S, A]) extends AnyVal {
    def asLens: Lens[S, A] = anIndexedLens.asLens_

    def asIndexedLens: IndexedLens[I, S, A] = anIndexedLens.asIndexedLens_
  }
}
