package proptics

import proptics.internal._

private[proptics] trait FoldCompat[S, A] extends FoldCompat0[S, A] with Fold1[S, A]
