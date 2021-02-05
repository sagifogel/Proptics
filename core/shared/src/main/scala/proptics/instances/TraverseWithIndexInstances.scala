package proptics.instances

import cats.data.NonEmptyList
import cats.instances.list._
import cats.instances.option._
import cats.syntax.functor._
import cats.{Applicative, Eval}

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
}
