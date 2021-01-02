package proptics.instances

import cats.data.NonEmptyList
import cats.instances.list._
import cats.{Applicative, Eval}

import proptics.indices.TraverseWithIndex
import proptics.instances.foldableWithIndex._
import proptics.instances.functorWithIndex._

trait TraverseWithIndexInstances {
  implicit val traverseWithIndexList: TraverseWithIndex[List, Int] = new TraverseWithIndex[List, Int] {
    override def foldLeftWithIndex[A, B](f: ((B, A), Int) => B)(fa: List[A], b: B): B =
      foldableWithIndexList.foldLeftWithIndex(f)(fa, b)

    override def foldRightWithIndex[A, B](f: ((A, Eval[B]), Int) => Eval[B])(fa: List[A], lb: Eval[B]): Eval[B] =
      foldableWithIndexList.foldRightWithIndex(f)(fa, lb)

    override def mapWithIndex[A, B](f: (A, Int) => B)(fa: List[A]): List[B] =
      functorWithIndexList.mapWithIndex(f)(fa)

    override def traverse[G[_], A, B](fa: List[A])(f: A => G[B])(implicit ev: Applicative[G]): G[List[B]] =
      catsStdInstancesForList.traverse(fa)(f)
  }

  implicit val traverseWithIndexNel: TraverseWithIndex[NonEmptyList, Int] = new TraverseWithIndex[NonEmptyList, Int] {
    override def foldLeftWithIndex[A, B](f: ((B, A), Int) => B)(fa: NonEmptyList[A], b: B): B =
      foldableWithIndexNel.foldLeftWithIndex(f)(fa, b)

    override def foldRightWithIndex[A, B](f: ((A, Eval[B]), Int) => Eval[B])(fa: NonEmptyList[A], lb: Eval[B]): Eval[B] =
      foldableWithIndexNel.foldRightWithIndex(f)(fa, lb)

    override def mapWithIndex[A, B](f: (A, Int) => B)(fa: NonEmptyList[A]): NonEmptyList[B] =
      functorWithIndexNel.mapWithIndex(f)(fa)

    override def traverse[G[_], A, B](fa: NonEmptyList[A])(f: A => G[B])(implicit ev: Applicative[G]): G[NonEmptyList[B]] =
      fa.traverse(f)
  }
}
