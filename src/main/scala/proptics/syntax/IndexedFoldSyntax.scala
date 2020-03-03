package proptics.syntax

import cats.Applicative
import cats.mtl.MonadState
import cats.syntax.apply._
import cats.syntax.option._
import proptics.IndexedFold
import proptics.internal.{Forget, Indexed}
import proptics.newtype.{Conj, Disj, Endo}
import proptics.syntax.FunctionSyntax._

object IndexedFoldSyntax {
  implicit class IndexedFoldTupledOps[I, S, T, A, B](val indexedFold: IndexedFold[(I, A), I, S, T, A, B]) extends AnyVal {
    def view(s: S): (I, A) = indexedFold(Indexed(Forget(identity))).runForget(s)

    def use[M[_]](implicit ev: MonadState[M, S]): M[(I, A)] = ev.inspect(indexedFold.view)
  }

  implicit class IndexedFoldrEndoOps[R, I, S, T, A, B](val indexedFold: IndexedFold[Endo[* => *, R], I, S, T, A, B]) extends AnyVal {
    def foldr(f: I => A => R => R)(r: R)(s: S): R =
      indexedFold.foldMap(i => Endo[* => *, R] _ compose f(i))(s).runEndo(r)
  }

  implicit class IndexedFoldlEndoOps[R, I, S, T, A, B](val indexedFold: IndexedFold[Endo[* => *, R], I, S, T, A, B]) extends AnyVal {
    def foldl(f: I => R => A => R)(r: R)(s: S): R =
      indexedFold.foldMap(i => Endo[* => *, R] _ compose f(i).flip)(s).runEndo(r)
  }

  implicit class IndexedFoldConjOps[R, I, S, T, A, B](val indexedFold: IndexedFold[Conj[R], I, S, T, A, B]) extends AnyVal {
    def all(f: I => A => R)(s: S): R =
      indexedFold.foldMap(i => Conj[R] _ compose f(i))(s).runConj
  }

  implicit class IndexedFoldDisjOps[R, I, S, T, A, B](val indexedFold: IndexedFold[Disj[R], I, S, T, A, B]) extends AnyVal {
    def any(f: I => A => R)(s: S): R =
      indexedFold.foldMap(i => Disj[R] _ compose f(i))(s).runDisj
  }

  implicit class IndexedFoldOptionOps[R, I, S, T, A, B](val indexedFold: IndexedFold[Endo[* => *, Option[A]], I, S, T, A, B]) extends AnyVal {
    def find(f: I => A => Boolean)(s: S): Option[A] =
      indexedFold.foldr(i => a => _.fold(if (f(i)(a)) a.some else None)(Some[A]))(None)(s)
  }

  implicit class IndexedFoldListOfTuplesOps[R, I, S, T, A, B](val indexedFold: IndexedFold[Endo[* => *, List[(I, A)]], I, S, T, A, B]) extends AnyVal {
    def toList(s: S): List[(I, A)] = indexedFold.foldr(i => a => xs => (i, a) :: xs)(Nil)(s)
  }

  implicit class IndexedFoldTraverseOpsOps[F[_], R, I, S, T, A, B](val indexedFold: IndexedFold[Endo[* => *, F[Unit]], I, S, T, A, B]) extends AnyVal {
    def traverse_(f: I => A => F[R])(s: S)(implicit ev: Applicative[F]): F[Unit] =
      indexedFold.foldr(i => a => ev.void(f(i)(a)) *> _)(ev.pure(()))(s)

    /**
     * Flipped version of `traverse_`.
     */
    def for_(s: S)(f: I => A => F[R])(implicit ev: Applicative[F]): F[Unit] = indexedFold.traverse_(f)(s)
  }
}
