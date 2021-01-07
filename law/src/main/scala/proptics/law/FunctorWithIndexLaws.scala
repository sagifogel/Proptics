package proptics.law

import cats.laws._

import proptics.indices.FunctorWithIndex
import proptics.syntax.functorWithIndex._

trait FunctorWithIndexLaws[F[_], I] extends FunctorLaws[F] {
  implicit def F: FunctorWithIndex[F, I]

  def composition[A, B, C](fa: F[A], f: (A, I) => B, g: (B, I) => C): IsEq[F[C]] =
    fa.mapWithIndex[I, B](f).mapWithIndex(g) <-> fa.mapWithIndex[I, C]((a, i) => g(f(a, i), i))

  def identity[A](fa: F[A]): IsEq[F[A]] =
    fa.mapWithIndex[I, A]((a, _) => a) <-> fa
}

object FunctorWithIndexLaws {
  def apply[F[_], I](implicit ev: FunctorWithIndex[F, I]): FunctorWithIndexLaws[F, I] = new FunctorWithIndexLaws[F, I] {
    implicit override def F: FunctorWithIndex[F, I] = ev
  }
}
