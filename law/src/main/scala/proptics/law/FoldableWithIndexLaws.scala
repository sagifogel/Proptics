package proptics.law

import cats.laws._
import cats.syntax.semigroup._
import cats.{Eval, Later, Monoid}

import proptics.indices.FoldableWithIndex
import proptics.syntax.foldableWithIndex._

trait FoldableWithIndexLaws[F[_], I] {
  implicit def F: FoldableWithIndex[F, I]

  def foldRightLazy[A](fa: F[A]): Boolean = {
    var i = 0
    F.foldRight(fa, Eval.now("empty")) { (_, _) =>
      i += 1
      Eval.now("not empty")
    }.value
    i == (if (F.isEmpty(fa)) 0 else 1)
  }

  def leftFoldConsistentWithFoldMap[A, B](
      fa: F[A],
      f: A => B
  )(implicit M: Monoid[B]): IsEq[B] =
    fa.foldMap(f) <-> fa.foldLeft(M.empty)((b, a) => b |+| f(a))

  def rightFoldConsistentWithFoldMap[A, B](
      fa: F[A],
      f: A => B
  )(implicit M: Monoid[B]): IsEq[B] =
    fa.foldMap(f) <-> fa.foldRight(Later(M.empty))((a, lb) => lb.map(f(a) |+| _)).value

  def foldRightWithIndexLazy[A](fa: F[A]): Boolean = {
    var i = 0
    F.foldRightWithIndex[A, String] { case (_, _) =>
      i += 1
      Eval.now("not empty")
    }(fa, Eval.now("empty"))
      .value

    i == (if (F.isEmpty(fa)) 0 else 1)
  }

  def leftFoldWithIndexConsistentWithFoldMapWithIndex[A, B](fa: F[A], f: (A, I) => B)(implicit ev: Monoid[B]): IsEq[B] =
    fa.foldMapWithIndex(f) <-> fa.foldLeftWithIndex[I, B] { case (b, (a, i)) =>
      b |+| f(a, i)
    }(ev.empty)

  def rightFoldWithIndexConsistentWithFoldMapWithIndex[A, B](fa: F[A], f: (A, I) => B)(implicit ev: Monoid[B]): IsEq[B] =
    fa.foldMapWithIndex(f) <-> fa
      .foldRightWithIndex[I, B] { case ((a, i), lb) =>
        lb.map(f(a, i) |+| _)
      }(Later(ev.empty))
      .value
}

object FoldableWithIndexLaws {
  def apply[F[_], I](implicit ev: FoldableWithIndex[F, I]): FoldableWithIndexLaws[F, I] = new FoldableWithIndexLaws[F, I] {
    implicit override def F: FoldableWithIndex[F, I] = ev
  }
}
