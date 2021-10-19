package proptics.applied

import proptics.FoldCompat0
import proptics.applied.internal.{AppliedFold0, AppliedGetter0}

private[proptics] trait AppliedFoldCompat0[S, A] extends AppliedFold0[S, A] with AppliedGetter0[S, A] {
  val value: S
  val optic: FoldCompat0[S, A]
}
