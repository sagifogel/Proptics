package proptics.syntax

import proptics.{ALens, AnIndexedLens_, IndexedLens_, Iso, Lens}

object AsLensSyntax {
  implicit class IsoAsLensOps[I, S, A](val iso: Iso[S, A]) extends AnyVal {
    def asLens: Lens[S, A] = iso.asLens_
  }

  implicit class ALensAsLensOps[I, S, A](val aLens: ALens[S, A]) extends AnyVal {
    def asLens: Lens[S, A] = aLens.asLens_
  }

  implicit class IndexedLensAsLensOps[I, S, A](val indexedLens: IndexedLens_[I, S, A]) extends AnyVal {
    def asLens: Lens[S, A] = indexedLens.asLens_
  }

  implicit class AnLensAsLensOps[I, S, A](val anIndexedLens: AnIndexedLens_[I, S, A]) extends AnyVal {
    def asLens: Lens[S, A] = anIndexedLens.asLens_
  }
}
