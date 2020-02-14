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
  implicit class FoldOpsA[S, T, A, B](val fold: Fold[A, S, T, A, B]) extends AnyVal {
    def view(s: S): A = fold(Forget(identity[A])).runForget(s)

    def use[M[_]](implicit ev: MonadState[M, S]): M[A] = ev.inspect(_ `^.` fold)

    def foldOf(s: S): A = fold.foldMapOf(identity)(s)
  }

  implicit class FoldFirstOps[S, T, A, B](val fold: Fold[First[A], S, T, A, B]) extends AnyVal {
    def preview(s: S): Option[A] = fold.foldMapOf(First[A] _ compose Some[A])(s).runFirst

    def firstOf(s: S): Option[A] = fold.preview(s)
  }

  implicit class FoldEndoOps[R, S, T, A, B](val fold: Fold[Endo[* => *, R], S, T, A, B]) extends AnyVal {
    def foldrOf(f: A => R => R)(r: R)(s: S): R = fold.foldMapOf(Endo[* => *, R] _ compose f)(s).runEndo(r)
  }

  implicit class FoldDualEndoOps[R, S, T, A, B](val fold: Fold[Dual[Endo[* => *, R]], S, T, A, B]) extends AnyVal {
    def foldlOf(f: R => A => R)(r: R)(s: S): R = {
      val dual = fold.foldMapOf(Dual[Endo[* => *, R]] _ compose Endo[* => *, R] compose f.flip)(s)

      dual.runDual.runEndo(r)
    }
  }

  implicit class FoldConjROps[R, S, T, A, B](val fold: Fold[Conj[R], S, T, A, B]) extends AnyVal {
    def allOf(f: A => R)(s: S): R = fold.foldMapOf(Conj[R] _ compose f)(s).runConj
  }

  implicit class FoldConjAOps[S, T, A, B](val fold: Fold[Conj[A], S, T, A, B]) extends AnyVal {
    def andOf(s: S): A = fold.allOf(identity[A])(s)
  }

  implicit class FoldDisjROps[R, S, T, A, B](val fold: Fold[Disj[R], S, T, A, B]) extends AnyVal {
    private def hasOrHasnt(s: S)(r: R)(implicit ev: Heyting[R]): R = fold.foldMapOf(const(Disj(r)))(s).runDisj

    def anyOf(f: A => R)(s: S): R = fold.foldMapOf(Disj[R] _ compose f)(s).runDisj

    def has(s: S)(implicit ev: Heyting[R]): R = hasOrHasnt(s)(ev.one)

    def hasnt(s: S)(implicit ev: Heyting[R]): R = hasOrHasnt(s)(ev.zero)
  }

  implicit class FoldDisjAOps[R, S, T, A, B](val fold: Fold[Disj[A], S, T, A, B]) extends AnyVal {
    def orOf(s: S): A = fold.anyOf(identity[A])(s)
  }

  implicit class FoldDisjBoolOps[R, S, T, A, B](val fold: Fold[Disj[Boolean], S, T, A, B]) extends AnyVal {
    def elemOf(a: A)(s: S)(implicit ev: Eq[A]): Boolean = fold.anyOf(_ === a)(s)

    def notElemOf(a: A)(s: S)(implicit ev: Eq[A]): Boolean = !elemOf(a)(s)
  }

  implicit class FoldAdditiveAOps[R, S, T, A, B](val fold: Fold[Additive[A], S, T, A, B]) extends AnyVal {
    def sumOf(s: S): A = fold.foldMapOf(Additive[A])(s).runAdditive
  }

  implicit class FoldMultiplicativeAOps[R, S, T, A, B](val fold: Fold[Multiplicative[A], S, T, A, B]) extends AnyVal {
    def productOf(s: S): A = fold.foldMapOf(Multiplicative[A])(s).runMultiplicative
  }

  implicit class FoldAdditiveIntOps[R, S, T, A, B](val fold: Fold[Additive[Int], S, T, A, B]) extends AnyVal {
    def lengthOf(s: S): Int = fold.foldMapOf(const(Additive(1)))(s).runAdditive
  }

  implicit class FoldLastOps[S, T, A, B](val fold: Fold[Last[A], S, T, A, B]) extends AnyVal {
    def lastOf(s: S): Option[A] = fold.foldMapOf(Last[A] _ compose Some[A])(s).runLast
  }

  implicit class FoldEndoOptionOps[S, T, A, B](val fold: Fold[Endo[* => *, Option[A]], S, T, A, B]) extends AnyVal {
    private def minMaxOf(s: S)(f: (A, A) => A)(implicit ev: Order[A]): Option[A] =
      fold.foldrOf(a => op => f(a, op.getOrElse(a)).some)(None)(s)

    def maximumOf(s: S)(implicit ev: Order[A]): Option[A] = minMaxOf(s)(ev.max)

    def minimumOf(s: S)(implicit ev: Order[A]): Option[A] = minMaxOf(s)(ev.min)

    def findOf(f: A => Boolean)(s: S): Option[A] =
      fold.foldrOf(a => _.fold(if (f(a)) a.some else None)(Some[A]))(None)(s)
  }

  implicit class FoldEndoSequenceOps[F[_], S, T, A, B](val fold: Fold[Endo[* => *, F[Unit]], S, T, F[A], B]) extends AnyVal {
    def sequenceOf_(s: S)(implicit ev: Applicative[F]): F[Unit] =
      fold.foldMapOf(f => Endo(f *> _))(s).runEndo(ev.pure(()))
  }

  implicit class FoldEndoTraverseOps[F[_], S, T, A, B](val fold: Fold[Endo[* => *, F[Unit]], S, T, A, B]) extends AnyVal {
    def traverseOf_[R](f: A => F[R])(s: S)(implicit ev: Applicative[F]): F[Unit] =
      fold.foldrOf(a => ev.void(f(a)) *> _)(ev.pure(()))(s)
  }

  implicit class FoldEndoListOps[S, T, A, B](val fold: Fold[Endo[* => *, List[A]], S, T, A, B]) extends AnyVal {
    def toListOf(s: S): List[A] = fold.foldrOf(x => x :: _)(Nil)(s)

    def toArrayOf[AA >: A](s: S)(implicit ev: ClassTag[AA]): Array[AA] = toListOf(s).toArray
  }
}