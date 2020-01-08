package optics.syntax

import cats.Applicative
import cats.syntax.option._
import cats.syntax.apply._
import cats.mtl.MonadState
import optics.IndexedOptic
import optics.internal.{Forget, Indexed}
import optics.newtype.{Conj, Disj, Endo}
import optics.syntax.FunctionSyntax._

import scala.Function.uncurried

object IndexedFoldSyntax {

  implicit class IndexedFoldOps[R, I, S, T, A, B](val indexedFold: IndexedOptic[Forget[R, *, *], I, S, T, A, B]) extends AnyVal {
    def foldMapOf(f: I => A => R)(s: S): R = indexedFold(Indexed(Forget(uncurried(f).tupled))).runForget(s)
  }

  implicit class IndexedFoldTupledOps[R, I, S, T, A, B](val indexedFold: IndexedOptic[Forget[(I, A), *, *], I, S, T, A, B]) extends AnyVal {
    def view(s: S): (I, A) = indexedFold(Indexed(Forget(identity))).runForget(s)

    def use[M[_]](implicit ev: MonadState[M, S]): M[(I, A)] = ev.inspect(indexedFold.view)

  }

  implicit class IndexedFoldrEndoOps[R, I, S, T, A, B](val indexedFold: IndexedOptic[Forget[Endo[* => *, R], *, *], I, S, T, A, B]) extends AnyVal {
    def foldrOf(f: I => A => R => R)(r: R)(s: S): R =
      indexedFold.foldMapOf(i => Endo[* => *, R] _ compose f(i))(s).runEndo(r)
  }

  implicit class IndexedFoldlEndoOps[R, I, S, T, A, B](val indexedFold: IndexedOptic[Forget[Endo[* => *, R], *, *], I, S, T, A, B]) extends AnyVal {
    def foldlOf(f: I => R => A => R)(r: R)(s: S): R =
      indexedFold.foldMapOf(i => Endo[* => *, R] _ compose f(i).flip)(s).runEndo(r)
  }

  implicit class IndexedFoldConjOps[R, I, S, T, A, B](val indexedFold: IndexedOptic[Forget[Conj[R], *, *], I, S, T, A, B]) extends AnyVal {
    def allOf(f: I => A => R)(s: S): R =
      indexedFold.foldMapOf(i => Conj[R] _ compose f(i))(s).runConj
  }

  implicit class IndexedFoldDisjOps[R, I, S, T, A, B](val indexedFold: IndexedOptic[Forget[Disj[R], *, *], I, S, T, A, B]) extends AnyVal {
    def anyOf(f: I => A => R)(s: S): R =
      indexedFold.foldMapOf(i => Disj[R] _ compose f(i))(s).runDisj
  }

  implicit class IndexedFoldOptionOps[R, I, S, T, A, B](val indexedFold: IndexedOptic[Forget[Endo[* => *, Option[A]], *, *], I, S, T, A, B]) extends AnyVal {
    def findOf(f: I => A => Boolean)(s: S): Option[A] =
      indexedFold.foldrOf(i => a => _.fold(if (f(i)(a)) a.some else None)(Some[A]))(None)(s)
  }

  implicit class IndexedFoldListOfTuplesOps[R, I, S, T, A, B](val indexedFold: IndexedOptic[Forget[Endo[* => *, List[(I, A)]], *, *], I, S, T, A, B]) extends AnyVal {
    def toListOf(s: S): List[(I, A)] = indexedFold.foldrOf(i => a => xs => (i, a) :: xs)(Nil)(s)
  }

  implicit class IndexedFoldTraverseOpsOps[F[_], R, I, S, T, A, B](val indexedFold: IndexedOptic[Forget[Endo[* => *, F[Unit]], *, *], I, S, T, A, B]) extends AnyVal {
    def traverseOf_(f: I => A => F[R])(s: S)(implicit ev: Applicative[F]): F[Unit] =
      indexedFold.foldrOf(i => a => ev.void(f(i)(a)) *> _)(ev.pure(()))(s)
  }
}
