package proptics

import proptics.internal.{Fold0, Getter0}

private[proptics] trait FoldCompat0[S, A] extends Fold0[S, A] with Getter0[S, A]