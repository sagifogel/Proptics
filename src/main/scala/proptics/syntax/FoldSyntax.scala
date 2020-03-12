package proptics.syntax

import algebra.lattice.Heyting
import cats.mtl.MonadState
import cats.syntax.apply._
import cats.syntax.eq._
import cats.syntax.option._
import cats.{Applicative, Eq, Order}
import proptics.Fold
import proptics.internal.Forget
import proptics.newtype._
import proptics.syntax.FoldOnSyntax._
import proptics.syntax.FunctionSyntax._

import scala.Function.const
import scala.reflect.ClassTag

object FoldSyntax {
  implicit class FoldOpsA[S, T, A, B](val _fold: Fold[A, S, T, A, B]) extends AnyVal {
    def view(s: S): A = _fold(Forget(identity[A])).runForget(s)

    def use[M[_]](implicit ev: MonadState[M, S]): M[A] = ev.inspect(_ `^.` _fold)

    def fold(s: S): A = _fold.foldMap(identity)(s)
  }

  implicit class FoldFirstOps[S, T, A, B](val fold: Fold[First[A], S, T, A, B]) extends AnyVal {
    def preview(s: S): Option[A] = fold.foldMap(First[A] _ compose Some[A])(s).runFirst

    def first(s: S): Option[A] = fold.preview(s)
  }

  implicit class FoldEndoOps[R, S, T, A, B](val fold: Fold[Endo[* => *, R], S, T, A, B]) extends AnyVal {
    def foldr(f: A => R => R)(r: R)(s: S): R = fold.foldMap(Endo[* => *, R] _ compose f)(s).runEndo(r)
  }

  implicit class FoldDualEndoOps[R, S, T, A, B](val fold: Fold[Dual[Endo[* => *, R]], S, T, A, B]) extends AnyVal {
    def foldl(f: R => A => R)(r: R)(s: S): R =
      fold.foldMap(Dual[Endo[* => *, R]] _ compose Endo[* => *, R] compose f.flip)(s).runDual.runEndo(r)
  }

  implicit class FoldConjROps[R, S, T, A, B](val fold: Fold[Conj[R], S, T, A, B]) extends AnyVal {
    def all(f: A => R)(s: S): R = fold.foldMap(Conj[R] _ compose f)(s).runConj
  }

  implicit class FoldConjAOps[S, T, A, B](val fold: Fold[Conj[A], S, T, A, B]) extends AnyVal {
    def and(s: S): A = fold.all(identity[A])(s)
  }

  implicit class FoldDisjROps[R, S, T, A, B](val fold: Fold[Disj[R], S, T, A, B]) extends AnyVal {
    private def hasOrHasnt(s: S)(r: R)(implicit ev: Heyting[R]): R = fold.foldMap(const(Disj(r)))(s).runDisj

    def any(f: A => R)(s: S): R = fold.foldMap(Disj[R] _ compose f)(s).runDisj

    def has(s: S)(implicit ev: Heyting[R]): R = hasOrHasnt(s)(ev.one)

    def hasNot(s: S)(implicit ev: Heyting[R]): R = hasOrHasnt(s)(ev.zero)
  }

  implicit class FoldDisjAOps[R, S, T, A, B](val fold: Fold[Disj[A], S, T, A, B]) extends AnyVal {
    def or(s: S): A = fold.any(identity[A])(s)
  }

  implicit class FoldDisjBoolOps[R, S, T, A, B](val fold: Fold[Disj[Boolean], S, T, A, B]) extends AnyVal {
    def contains(a: A)(s: S)(implicit ev: Eq[A]): Boolean = fold.any(_ === a)(s)

    def notContains(a: A)(s: S)(implicit ev: Eq[A]): Boolean = !contains(a)(s)
  }

  implicit class FoldAdditiveAOps[R, S, T, A, B](val fold: Fold[Additive[A], S, T, A, B]) extends AnyVal {
    def sum(s: S): A = fold.foldMap(Additive[A])(s).runAdditive
  }

  implicit class FoldMultiplicativeAOps[R, S, T, A, B](val fold: Fold[Multiplicative[A], S, T, A, B]) extends AnyVal {
    def product(s: S): A = fold.foldMap(Multiplicative[A])(s).runMultiplicative
  }

  implicit class FoldAdditiveIntOps[R, S, T, A, B](val fold: Fold[Additive[Int], S, T, A, B]) extends AnyVal {
    def length(s: S): Int = fold.foldMap(const(Additive(1)))(s).runAdditive
  }

  implicit class FoldLastOps[S, T, A, B](val fold: Fold[Last[A], S, T, A, B]) extends AnyVal {
    def last(s: S): Option[A] = fold.foldMap(Last[A] _ compose Some[A])(s).runLast
  }

  implicit class FoldEndoOptionOps[S, T, A, B](val fold: Fold[Endo[* => *, Option[A]], S, T, A, B]) extends AnyVal {
    private def minMax(s: S)(f: (A, A) => A)(implicit ev: Order[A]): Option[A] =
      fold.foldr(a => op => f(a, op.getOrElse(a)).some)(None)(s)

    def maximum(s: S)(implicit ev: Order[A]): Option[A] = minMax(s)(ev.max)

    def minimum(s: S)(implicit ev: Order[A]): Option[A] = minMax(s)(ev.min)

    def find(f: A => Boolean)(s: S): Option[A] =
      fold.foldr(a => _.fold(if (f(a)) a.some else None)(Some[A]))(None)(s)
  }

  implicit class FoldEndoSequenceOps[F[_], S, T, A, B](val fold: Fold[Endo[* => *, F[Unit]], S, T, F[A], B]) extends AnyVal {
    def sequence_(s: S)(implicit ev: Applicative[F]): F[Unit] =
      fold.foldMap(f => Endo(f *> _))(s).runEndo(ev.pure(()))
  }

  implicit class FoldEndoTraverseOps[F[_], S, T, A, B](val fold: Fold[Endo[* => *, F[Unit]], S, T, A, B]) extends AnyVal {
    def traverse_[R](f: A => F[R])(s: S)(implicit ev: Applicative[F]): F[Unit] =
      fold.foldr(a => ev.void(f(a)) *> _)(ev.pure(()))(s)
  }

  implicit class FoldEndoListOps[S, T, A, B](val fold: Fold[Endo[* => *, List[A]], S, T, A, B]) extends AnyVal {
    def toList(s: S): List[A] = fold.foldr(x => x :: _)(Nil)(s)

    def toArray[AA >: A](s: S)(implicit ev: ClassTag[AA]): Array[AA] = toList(s).toArray
  }
}