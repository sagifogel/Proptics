package proptics

import proptics.internal._

trait FoldCompat[S, A] extends FoldCompat0[S, A] with Fold1[S, A]
