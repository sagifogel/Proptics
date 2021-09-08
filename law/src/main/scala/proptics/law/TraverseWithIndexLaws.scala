package proptics.law

import cats.laws.{IsEq, TraverseLaws, _}
import cats.{Id, catsInstancesForId}

import proptics.indices.TraverseWithIndex
import proptics.syntax.functorWithIndex._
import proptics.syntax.traverseWithIndex._

trait TraverseWithIndexLaws[F[_], I] extends TraverseLaws[F] {
  implicit def F: TraverseWithIndex[F, I]

  def traverseWithIndexIdentity[A, B](fa: F[A], f: (A, I) => B): IsEq[F[B]] =
    fa.traverseWithIndex[Id, I, B](f) <-> fa.mapWithIndex[I, B](f)

  def traverseWithIndexComposition[A, B, C](fa: F[A], f: (A, I) => B, g: (B, I) => C): IsEq[F[C]] = {
    val fb: F[B] = fa.traverseWithIndex[Id, I, B](f)

    fb.traverseWithIndex[Id, I, C](g) <->
      fa.traverseWithIndex[Id, I, C]((a, i) => g(f(a, i), i))
  }
}

object TraverseWithIndexLaws {
  def apply[F[_], I](implicit ev: TraverseWithIndex[F, I]): TraverseWithIndexLaws[F, I] = new TraverseWithIndexLaws[F, I] {
    implicit override def F: TraverseWithIndex[F, I] = ev
  }
}
