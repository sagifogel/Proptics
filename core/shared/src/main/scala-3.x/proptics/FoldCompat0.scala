package proptics

import cats.Monoid

trait FoldCompat0[S, A] extends Serializable {
  /** map each focus of a [[Fold_]] to a [[cats.Monoid]], and combine the results */
  protected[proptics] def foldMap[R: Monoid](s: S)(f: A => R): R
}
