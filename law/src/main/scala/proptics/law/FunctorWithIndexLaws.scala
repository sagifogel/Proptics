package proptics.law

import cats.laws._

import proptics.indices.FunctorWithIndex
import proptics.syntax.functorWithIndex._

trait FunctorWithIndexLaws[F[_], I] {
  implicit def F: FunctorWithIndex[F, I]

  def functorWithIndexComposition[A, B, C](fa: F[A], f: (A, I) => B, g: (B, I) => C): IsEq[F[C]] =
    fa.mapWithIndex[I, B](f).mapWithIndex(g) <-> fa.mapWithIndex[I, C]((a, i) => g(f(a, i), i))

  def functorWithIndexIdentity[A](fa: F[A]): IsEq[F[A]] =
    fa.mapWithIndex[I, A]((a, _) => a) <-> fa

  def covariantIdentity[A](fa: F[A]): IsEq[F[A]] =
    fa.map(identity) <-> fa

  def covariantComposition[A, B, C](fa: F[A], f: A => B, g: B => C): IsEq[F[C]] =
    fa.map(f).map(g) <-> fa.map(f.andThen(g))
}

object FunctorWithIndexLaws {
  def apply[F[_], I](implicit ev: FunctorWithIndex[F, I]): FunctorWithIndexLaws[F, I] = new FunctorWithIndexLaws[F, I] {
    implicit override def F: FunctorWithIndex[F, I] = ev
  }
}
