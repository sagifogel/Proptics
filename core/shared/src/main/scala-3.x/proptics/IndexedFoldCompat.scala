package proptics

import proptics.internal.{IndexedFold0, IndexedGetter0}

private[proptics] trait IndexedFoldCompat[I, S, A] extends IndexedFold0[I, S, A] with IndexedGetter0[I, S, A]
