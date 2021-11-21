package proptics.syntax.applied

import cats.data.State
import cats.{Applicative, Bifoldable, Eq, Foldable, Traverse}

import proptics.applied.{AppliedFold, AppliedFold_}
import proptics.syntax.fold._
import proptics.{AffineTraversal, AppliedFold, Fold, Fold_, Prism}

trait AppliedFoldSyntax {
  implicit final def appliedFoldOpsWithFoldable[F[_], A](s: F[A]): AppliedFoldOpsWithFoldable[F, A] = AppliedFoldOpsWithFoldable(s)

  implicit final def appliedFoldElementOps[S, T, A](traversal: AppliedFold_[S, T, A, A]): AppliedFoldElementOps[S, T, A] = AppliedFoldElementOps(traversal)

  implicit final def appliedFoldWithFoldableFocusElementOps[F[_], S, T, A](appliedFold: AppliedFold_[S, T, F[A], F[A]]): AppliedFoldWithFoldableFocusElementOps[F, S, T, A] =
    AppliedFoldWithFoldableFocusElementOps(appliedFold)

  implicit final def appliedBifoldablelElementOps[G[_, _], A](s: G[A, A]): AppliedBifoldableElementOps[G, A] = AppliedBifoldableElementOps(s)
}

final case class AppliedFoldOpsWithFoldable[F[_], A](private val s: F[A]) extends AnyVal {
  def foldable_[B, T](implicit ev: Foldable[F]): AppliedFold_[F[A], B, A, T] =
    AppliedFold_(s, Fold_.fromFoldable[F, A, B, T])

  def foldable(implicit ev: Foldable[F]): AppliedFold[F[A], A] =
    AppliedFold(s, Fold.fromFoldable[F, A])
}

final case class AppliedFoldElementOps[S, T, A](private val appliedFold: AppliedFold_[S, T, A, A]) extends AnyVal {
  def value: S = appliedFold.value
  def optic: Fold_[S, T, A, A] = appliedFold.optic

  /** narrow the focus of a [[Fold_]] focus a single element */
  def elementAt(i: Int): AppliedFold_[S, T, A, A] = AppliedFold_(value, optic.elementAt(i))

  /** traverse elements of a [[Fold_]] whose index satisfy a predicate */
  def filterByIndex(predicate: Int => Boolean): AppliedFold_[S, T, A, A] =
    AppliedFold_(value, optic.filterByIndex(predicate))

  def take(i: Int): AppliedFold_[S, T, A, A] = AppliedFold_(value, optic.take(i))

  /** select all elements of a [[Fold_]] except first n ones */
  def drop(i: Int): AppliedFold_[S, T, A, A] = AppliedFold_(value, optic.drop(i))

  /** take longest prefix of elements of a [[Fold_]] that satisfy a predicate */
  def takeWhile(predicate: A => Boolean)(implicit ev0: Applicative[State[Boolean, *]]): AppliedFold_[S, T, A, A] =
    AppliedFold_(value, optic.takeWhile(predicate))

  /** drop longest prefix of elements of a [[Fold_]] that satisfy a predicate */
  def dropWhile(predicate: A => Boolean)(implicit ev0: Applicative[State[Boolean, *]]): AppliedFold_[S, T, A, A] =
    AppliedFold_(value, optic.dropWhile(predicate))

  /** filter out elements that do not match the predicate, of optics composed with this [[Fold_]] */
  def filter(predicate: A => Boolean): AppliedFold_[S, T, A, A] =
    AppliedFold_(value, optic.andThen(Fold.filter[A](predicate)))

  /** filter out elements that match the predicate, of optics composed with this [[Fold_]] */
  def filterNot(predicate: A => Boolean): AppliedFold_[S, T, A, A] =
    filter(a => !predicate(a))

  /** filter out elements using a [[Fold]], of optics composed with this [[Fold_]] */
  def filterF[B](fold: Fold[A, B]): AppliedFold_[S, T, A, A] =
    AppliedFold_(value, optic.andThen(Fold.filter[A, B](fold)))

  /** filter out elements using an [[AffineTraversal]], of optics composed with this [[Fold_]] */
  def filterF[B](fold: AffineTraversal[A, B]): AppliedFold_[S, T, A, A] =
    AppliedFold_(value, optic.andThen(Fold.filter[A, B](fold)))

  /** filter out elements using a [[Fold]], of optics composed with this [[Fold_]] */
  def onlyWhen[B: Eq](f: A => B, is: B): AppliedFold_[S, T, A, A] = {
    val foldPredicate = Fold(f).andThen(Prism.only[B](is))

    AppliedFold_(appliedFold.value, appliedFold.optic.andThen(Fold.filter(foldPredicate)))
  }
}

final case class AppliedFoldWithFoldableFocusElementOps[F[_], S, T, A](private val appliedFold: AppliedFold_[S, T, F[A], F[A]]) extends AnyVal {
  def value: S = appliedFold.value
  def optic: Fold_[S, T, F[A], F[A]] = appliedFold.optic

  /** select the first n elements of a [[Fold]] */
  def take(i: Int)(implicit ev: Foldable[F]): AppliedFold_[S, T, A, A] =
    AppliedFold_(value, optic.andThen(Fold.take[F, A](i)))

  /** select all elements of a [[Fold]] except first n ones */
  def drop(i: Int)(implicit ev: Traverse[F]): AppliedFold_[S, T, A, A] =
    AppliedFold_(value, optic.andThen(Fold.drop[F, A](i)))

  /** take longest prefix of elements of a [[Fold]] that satisfy a predicate */
  def takeWhile(predicate: A => Boolean)(implicit ev0: Applicative[State[Boolean, *]], ev1: Foldable[F]): AppliedFold_[S, T, A, A] =
    AppliedFold_(value, optic.andThen(Fold.takeWhile[F, A](predicate)))

  /** drop longest prefix of elements of a [[Fold]] that satisfy a predicate */
  def dropWhile(predicate: A => Boolean)(implicit ev0: Applicative[State[Boolean, *]], ev1: Foldable[F]): AppliedFold_[S, T, A, A] =
    AppliedFold_(value, optic.andThen(Fold.dropWhile[F, A](predicate)))

  def andThenFold(implicit ev: Foldable[F]): AppliedFold_[S, T, A, A] =
    AppliedFold_(value, optic.andThen(Fold.fromFoldable[F, A]))
}

final case class AppliedBifoldableElementOps[G[_, _], A](private val s: G[A, A]) extends AnyVal {
  def bifold_[B](implicit ev: Bifoldable[G]): AppliedFold_[G[A, A], G[B, B], A, B] =
    AppliedFold_(s, Fold_.both[G, A, B])

  def bifold(implicit ev: Bifoldable[G]): AppliedFold[G[A, A], A] =
    AppliedFold(s, Fold.both[G, A])
}
