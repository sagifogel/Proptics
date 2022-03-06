package proptics.applied

import proptics.FoldCompat
import proptics.applied.internal.AppliedFold1

private[proptics] trait AppliedFoldCompat[S, A] extends AppliedFoldCompat0[S, A] with AppliedFold1[S, A] {
  val value: S
  val optic: FoldCompat[S, A]
}
