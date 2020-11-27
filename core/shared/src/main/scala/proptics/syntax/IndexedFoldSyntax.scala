package proptics.syntax

import cats.syntax.eq._
import cats.syntax.semigroup._
import cats.{Eq, Monoid}

import proptics.IndexedFold_
import proptics.internal.{Forget, Indexed}
import proptics.syntax.tuple._

trait IndexedFoldSyntax {
  implicit def indexedFoldOps[I, S, T, A](indexedFold: IndexedFold_[I, S, T, A, A]): IndexedFoldsOps[I, S, T, A] =
    IndexedFoldsOps(indexedFold)
}

final case class IndexedFoldsOps[I, S, T, A](private val indexedFold: IndexedFold_[I, S, T, A, A]) extends AnyVal {
  /** combine an index and an [[IndexedFold_]] to narrow the focus to a single element */
  def element(i: I)(implicit ev: Eq[I]): IndexedFold_[I, S, T, A, A] = filterByIndex(_ === i)

  /** traverse elements of an [[IndexedFold_]] whose index satisfy a predicate applied on the index */
  def filterByIndex(predicate: I => Boolean): IndexedFold_[I, S, T, A, A] = filter(predicate compose Tuple2._1)

  /** traverse elements of an [[IndexedFold_]] whose index satisfy a predicate */
  def filter(predicate: ((I, A)) => Boolean): IndexedFold_[I, S, T, A, A] = new IndexedFold_[I, S, T, A, A] {
    def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, A, A]): Forget[R, S, T] = {
      val runForget = indexed.runIndex.runForget

      Forget(s =>
        indexedFold.foldl(s)(Monoid[R].empty) { (r, pair) =>
          if (predicate(pair)) r |+| runForget(pair) else r
        })
    }
  }
}
