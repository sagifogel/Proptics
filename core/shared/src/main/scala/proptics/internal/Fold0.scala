package proptics.internal

import cats.Monoid
import cats.syntax.option._

import proptics.data.{Disj, First}

private[proptics] trait Fold0[S, A] extends FoldInstances {
  /** map each focus of a Fold to a [[cats.Monoid]], and combine the results */
  protected def foldMap[R: Monoid](s: S)(f: A => R): R

  protected def viewOption(s: S): Option[A] = foldMap(s)(a => First(a.some)).runFirst

  /** view the first focus of a Fold, if there is any */
  final def preview(s: S): Option[A] = viewOption(s)

  /** test whether a predicate holds for the focus of a Getter */
  final def exists(f: A => Boolean): S => Boolean = foldMap(_)(Disj[Boolean] _ compose f).runDisj

  /** test whether there is no focus or a predicate holds for the focus of a Fold */
  def forall(f: A => Boolean): S => Boolean = preview(_).forall(f)
}
