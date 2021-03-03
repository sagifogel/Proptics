package proptics.instances

import scala.collection.immutable.ListMap

import cats.data.NonEmptyVector._
import cats.data.{NonEmptyList, NonEmptyMap, NonEmptyVector}
import cats.instances.list._
import cats.instances.option._
import cats.instances.vector._
import cats.syntax.functor._
import cats.{Applicative, Eval, Order}

import proptics.indices.TraverseWithIndex
import proptics.instances.foldableWithIndex._
import proptics.instances.functorWithIndex._

trait TraverseWithIndexInstances {
  implicit val traverseWithIndexOption: TraverseWithIndex[Option, Unit] = new TraverseWithIndex[Option, Unit] {
    override def mapWithIndex[A, B](f: (A, Unit) => B)(fa: Option[A]): Option[B] =
      functorWithIndexOption.mapWithIndex(f)(fa)

    override def traverse[G[_], A, B](fa: Option[A])(f: A => G[B])(implicit ev: Applicative[G]): G[Option[B]] =
      catsStdInstancesForOption.traverse(fa)(f)

    override def foldLeftWithIndex[A, B](f: (B, (A, Unit)) => B)(fa: Option[A], b: B): B =
      foldableWithIndexOption.foldLeftWithIndex(f)(fa, b)

    override def foldRightWithIndex[A, B](f: ((A, Unit), Eval[B]) => Eval[B])(fa: Option[A], lb: Eval[B]): Eval[B] =
      foldableWithIndexOption.foldRightWithIndex(f)(fa, lb)
  }

  implicit val traverseWithIndexVector: TraverseWithIndex[Vector, Int] = new TraverseWithIndex[Vector, Int] {
    override def mapWithIndex[A, B](f: (A, Int) => B)(fa: Vector[A]): Vector[B] =
      functorWithIndexVector.mapWithIndex(f)(fa)

    override def foldLeftWithIndex[A, B](f: (B, (A, Int)) => B)(fa: Vector[A], b: B): B =
      foldableWithIndexVector.foldLeftWithIndex(f)(fa, b)

    override def foldRightWithIndex[A, B](f: ((A, Int), Eval[B]) => Eval[B])(fa: Vector[A], lb: Eval[B]): Eval[B] =
      foldableWithIndexVector.foldRightWithIndex(f)(fa, lb)

    override def traverse[G[_], A, B](fa: Vector[A])(f: A => G[B])(implicit ev: Applicative[G]): G[Vector[B]] =
      catsStdInstancesForVector.traverse(fa)(f)
  }

  implicit val traverseWithIndexList: TraverseWithIndex[List, Int] = new TraverseWithIndex[List, Int] {
    override def mapWithIndex[A, B](f: (A, Int) => B)(fa: List[A]): List[B] =
      functorWithIndexList.mapWithIndex(f)(fa)

    override def foldLeftWithIndex[A, B](f: (B, (A, Int)) => B)(fa: List[A], b: B): B =
      foldableWithIndexList.foldLeftWithIndex(f)(fa, b)

    override def foldRightWithIndex[A, B](f: ((A, Int), Eval[B]) => Eval[B])(fa: List[A], lb: Eval[B]): Eval[B] =
      foldableWithIndexList.foldRightWithIndex(f)(fa, lb)

    override def traverse[G[_], A, B](fa: List[A])(f: A => G[B])(implicit ev: Applicative[G]): G[List[B]] =
      catsStdInstancesForList.traverse(fa)(f)
  }

  implicit def traverseWithIndexMap[K]: TraverseWithIndex[Map[K, *], K] = new TraverseWithIndex[Map[K, *], K] {
    override def mapWithIndex[A, B](f: (A, K) => B)(fa: Map[K, A]): Map[K, B] =
      functorWithIndexMap.mapWithIndex(f)(fa)

    override def foldLeftWithIndex[A, B](f: (B, (A, K)) => B)(fa: Map[K, A], b: B): B =
      foldableWithIndexMap.foldLeftWithIndex(f)(fa, b)

    override def foldRightWithIndex[A, B](f: ((A, K), Eval[B]) => Eval[B])(fa: Map[K, A], lb: Eval[B]): Eval[B] =
      foldableWithIndexMap.foldRightWithIndex(f)(fa, lb)

    override def traverse[G[_], A, B](fa: Map[K, A])(f: A => G[B])(implicit ev: Applicative[G]): G[Map[K, B]] =
      catsStdInstancesForList
        .traverse(fa.toList) { case (key, value) => f(value).map(key -> _) }
        .map(_.toMap)
  }

  implicit def traverseWithIndexListMap[K]: TraverseWithIndex[ListMap[K, *], K] = new TraverseWithIndex[ListMap[K, *], K] {
    override def mapWithIndex[A, B](f: (A, K) => B)(fa: ListMap[K, A]): ListMap[K, B] =
      functorWithIndexListMap.mapWithIndex(f)(fa)

    override def foldLeftWithIndex[A, B](f: (B, (A, K)) => B)(fa: ListMap[K, A], b: B): B =
      foldableWithIndexListMap.foldLeftWithIndex(f)(fa, b)

    override def foldRightWithIndex[A, B](f: ((A, K), Eval[B]) => Eval[B])(fa: ListMap[K, A], lb: Eval[B]): Eval[B] =
      foldableWithIndexListMap.foldRightWithIndex(f)(fa, lb)

    override def traverse[G[_], A, B](fa: ListMap[K, A])(f: A => G[B])(implicit ev: Applicative[G]): G[ListMap[K, B]] =
      catsStdInstancesForList
        .traverse(fa.iterator.toList) { case (key, value) => f(value).map(key -> _) }
        .map(ls => ListMap(ls: _*))
  }

  implicit val traverseWithIndexNev: TraverseWithIndex[NonEmptyVector, Int] = new TraverseWithIndex[NonEmptyVector, Int] {
    override def mapWithIndex[A, B](f: (A, Int) => B)(fa: NonEmptyVector[A]): NonEmptyVector[B] =
      functorWithIndexNev.mapWithIndex(f)(fa)

    override def foldLeftWithIndex[A, B](f: (B, (A, Int)) => B)(fa: NonEmptyVector[A], b: B): B =
      foldableWithIndexNev.foldLeftWithIndex(f)(fa, b)

    override def foldRightWithIndex[A, B](f: ((A, Int), Eval[B]) => Eval[B])(fa: NonEmptyVector[A], lb: Eval[B]): Eval[B] =
      foldableWithIndexNev.foldRightWithIndex(f)(fa, lb)

    override def traverse[G[_], A, B](fa: NonEmptyVector[A])(f: A => G[B])(implicit ev: Applicative[G]): G[NonEmptyVector[B]] =
      catsDataInstancesForNonEmptyVector.traverse(fa)(f)
  }

  implicit val traverseWithIndexNel: TraverseWithIndex[NonEmptyList, Int] = new TraverseWithIndex[NonEmptyList, Int] {
    override def mapWithIndex[A, B](f: (A, Int) => B)(fa: NonEmptyList[A]): NonEmptyList[B] =
      functorWithIndexNel.mapWithIndex(f)(fa)

    override def foldLeftWithIndex[A, B](f: (B, (A, Int)) => B)(fa: NonEmptyList[A], b: B): B =
      foldableWithIndexNel.foldLeftWithIndex(f)(fa, b)

    override def foldRightWithIndex[A, B](f: ((A, Int), Eval[B]) => Eval[B])(fa: NonEmptyList[A], lb: Eval[B]): Eval[B] =
      foldableWithIndexNel.foldRightWithIndex(f)(fa, lb)

    override def traverse[G[_], A, B](fa: NonEmptyList[A])(f: A => G[B])(implicit ev: Applicative[G]): G[NonEmptyList[B]] =
      fa.traverse(f)
  }

  implicit def traverseWithIndexNonEmptyMap[K: Order]: TraverseWithIndex[NonEmptyMap[K, *], K] = new TraverseWithIndex[NonEmptyMap[K, *], K] {
    override def mapWithIndex[A, B](f: (A, K) => B)(fa: NonEmptyMap[K, A]): NonEmptyMap[K, B] =
      functorWithIndexNem[K].mapWithIndex[A, B](f)(fa)

    override def foldLeftWithIndex[A, B](f: (B, (A, K)) => B)(fa: NonEmptyMap[K, A], b: B): B =
      foldableWithIndexNem.foldLeftWithIndex(f)(fa, b)

    override def foldRightWithIndex[A, B](f: ((A, K), Eval[B]) => Eval[B])(fa: NonEmptyMap[K, A], lb: Eval[B]): Eval[B] =
      foldableWithIndexNem.foldRightWithIndex(f)(fa, lb)

    override def traverse[G[_], A, B](fa: NonEmptyMap[K, A])(f: A => G[B])(implicit ev: Applicative[G]): G[NonEmptyMap[K, B]] =
      fa.nonEmptyTraverse(f)
  }
}
