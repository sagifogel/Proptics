package proptics.syntax

import cats.Monoid
import cats.syntax.eq._
import cats.syntax.semigroup._

import proptics.Fold_
import proptics.internal.Forget
import proptics.rank2types.Rank2TypeFoldLike
import proptics.syntax.indexedFold._

trait FoldSyntax {
  implicit final def foldElementOps[S, T, A](fold: Fold_[S, T, A, A]): FoldElementOps[S, T, A] = FoldElementOps(fold)
}

final case class FoldElementOps[S, T, A](private val fold: Fold_[S, T, A, A]) extends AnyVal {
  /** narrow the focus of a [[Fold_]] to a single element */
  def element(i: Int): Fold_[S, T, A, A] = filterByIndex(_ === i)

  /** traverse elements of a [[Fold_]] whose index satisfy a predicate */
  def filterByIndex(predicate: Int => Boolean): Fold_[S, T, A, A] =
    fold.asIndexableFold.filterByIndex(predicate).unIndex

  /** select the first n elements of a [[Fold_]] */
  def take(i: Int): Fold_[S, T, A, A] = filterByIndex(_ < i)

  /** select all elements of a [[Fold_]] except first n ones */
  def drop(i: Int): Fold_[S, T, A, A] = filterByIndex(_ >= i)

  /** take longest prefix of elements of a [[Fold_]] that satisfy a predicate */
  def takeWhile(predicate: A => Boolean): Fold_[S, T, A, A] =
    foldWhile(predicate, take = true)

  /** drop longest prefix of elements of a [[Fold_]] that satisfy a predicate */
  def dropWhile(predicate: A => Boolean): Fold_[S, T, A, A] =
    foldWhile(predicate, take = false)

  private[FoldElementOps] def foldWhile(predicate: A => Boolean, take: Boolean): Fold_[S, T, A, A] =
    Fold_(new Rank2TypeFoldLike[S, T, A, A] {
      override def apply[R: Monoid](forget: Forget[R, A, A]): Forget[R, S, T] = {
        val runForget = forget.runForget

        Forget(s =>
          fold
            .foldLeft(s)((true, Monoid[R].empty)) { case ((b, r), a) =>
              val acc = b && predicate(a)
              val result =
                if (acc)
                  if (take) r |+| runForget(a) else r
                else if (take) r
                else r |+| runForget(a)
              (acc, result)
            }
            ._2)
      }
    })
}
