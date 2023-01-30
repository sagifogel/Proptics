package proptics.instances

import scala.collection.immutable.ListMap

import cats.data.Chain._
import cats.data.NonEmptyChain._
import cats.data.NonEmptyVector._
import cats.data.OneAnd._
import cats.data._
import cats.instances.list._
import cats.instances.option._
import cats.instances.vector._
import cats.syntax.functor._
import cats.{Applicative, Eval, Order, Traverse}

import proptics.indices.TraverseWithIndex
import proptics.instances.foldableWithIndex._
import proptics.instances.functorWithIndex._

trait TraverseWithIndexInstances extends ScalaVersionSpecificTraverseWithIndexInstances {
  implicit final val traverseWithIndexOption: TraverseWithIndex[Option, Unit] = new TraverseWithIndex[Option, Unit] {
    override def mapWithIndex[A, B](f: (A, Unit) => B)(fa: Option[A]): Option[B] =
      functorWithIndexOption.mapWithIndex(f)(fa)

    override def traverse[G[_], A, B](fa: Option[A])(f: A => G[B])(implicit ev: Applicative[G]): G[Option[B]] =
      catsStdInstancesForOption.traverse(fa)(f)

    override def foldLeftWithIndex[A, B](f: (B, (A, Unit)) => B)(fa: Option[A], b: B): B =
      foldableWithIndexOption.foldLeftWithIndex(f)(fa, b)

    override def foldRightWithIndex[A, B](f: ((A, Unit), Eval[B]) => Eval[B])(fa: Option[A], lb: Eval[B]): Eval[B] =
      foldableWithIndexOption.foldRightWithIndex(f)(fa, lb)

    override def exists[A](fa: Option[A])(f: A => Boolean): Boolean = foldableWithIndexOption.exists(fa)(f)
  }

  implicit final val traverseWithIndexVector: TraverseWithIndex[Vector, Int] = new TraverseWithIndex[Vector, Int] {
    override def mapWithIndex[A, B](f: (A, Int) => B)(fa: Vector[A]): Vector[B] =
      functorWithIndexVector.mapWithIndex(f)(fa)

    override def foldLeftWithIndex[A, B](f: (B, (A, Int)) => B)(fa: Vector[A], b: B): B =
      foldableWithIndexVector.foldLeftWithIndex(f)(fa, b)

    override def foldRightWithIndex[A, B](f: ((A, Int), Eval[B]) => Eval[B])(fa: Vector[A], lb: Eval[B]): Eval[B] =
      foldableWithIndexVector.foldRightWithIndex(f)(fa, lb)

    override def traverse[G[_], A, B](fa: Vector[A])(f: A => G[B])(implicit ev: Applicative[G]): G[Vector[B]] =
      catsStdInstancesForVector.traverse(fa)(f)

    override def exists[A](fa: Vector[A])(f: A => Boolean): Boolean = foldableWithIndexVector.exists(fa)(f)
  }

  implicit final val traverseWithIndexList: TraverseWithIndex[List, Int] = new TraverseWithIndex[List, Int] {
    override def mapWithIndex[A, B](f: (A, Int) => B)(fa: List[A]): List[B] =
      functorWithIndexList.mapWithIndex(f)(fa)

    override def foldLeftWithIndex[A, B](f: (B, (A, Int)) => B)(fa: List[A], b: B): B =
      foldableWithIndexList.foldLeftWithIndex(f)(fa, b)

    override def foldRightWithIndex[A, B](f: ((A, Int), Eval[B]) => Eval[B])(fa: List[A], lb: Eval[B]): Eval[B] =
      foldableWithIndexList.foldRightWithIndex(f)(fa, lb)

    override def traverse[G[_], A, B](fa: List[A])(f: A => G[B])(implicit ev: Applicative[G]): G[List[B]] =
      catsStdInstancesForList.traverse(fa)(f)

    override def exists[A](fa: List[A])(f: A => Boolean): Boolean = foldableWithIndexList.exists(fa)(f)
  }

  implicit final def traverseWithIndexListMap[K]: TraverseWithIndex[ListMap[K, *], K] = new TraverseWithIndex[ListMap[K, *], K] {
    override def mapWithIndex[A, B](f: (A, K) => B)(fa: ListMap[K, A]): ListMap[K, B] =
      functorWithIndexListMap.mapWithIndex(f)(fa)

    override def foldLeftWithIndex[A, B](f: (B, (A, K)) => B)(fa: ListMap[K, A], b: B): B =
      foldableWithIndexListMap.foldLeftWithIndex(f)(fa, b)

    override def foldRightWithIndex[A, B](f: ((A, K), Eval[B]) => Eval[B])(fa: ListMap[K, A], lb: Eval[B]): Eval[B] =
      foldableWithIndexListMap.foldRightWithIndex(f)(fa, lb)

    override def traverse[G[_], A, B](fa: ListMap[K, A])(f: A => G[B])(implicit ev: Applicative[G]): G[ListMap[K, B]] =
      catsStdInstancesForList
        .traverse(fa.toList) { case (key, value) => f(value).map(key -> _) }
        .map(ListMap(_: _*))

    override def exists[A](fa: ListMap[K, A])(f: A => Boolean): Boolean = foldableWithIndexListMap.exists(fa)(f)
  }

  implicit final def traverseWithIndexMap[K]: TraverseWithIndex[Map[K, *], K] = new TraverseWithIndex[Map[K, *], K] {
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

    override def exists[A](fa: Map[K, A])(f: A => Boolean): Boolean = foldableWithIndexMap.exists(fa)(f)
  }

  implicit final val traverseWithIndexChain: TraverseWithIndex[Chain, Int] = new TraverseWithIndex[Chain, Int] {
    override def mapWithIndex[A, B](f: (A, Int) => B)(fa: Chain[A]): Chain[B] =
      functorWithIndexChain.mapWithIndex(f)(fa)

    override def foldLeftWithIndex[A, B](f: (B, (A, Int)) => B)(fa: Chain[A], b: B): B =
      foldableWithIndexChain.foldLeftWithIndex(f)(fa, b)

    override def foldRightWithIndex[A, B](f: ((A, Int), Eval[B]) => Eval[B])(fa: Chain[A], lb: Eval[B]): Eval[B] =
      foldableWithIndexChain.foldRightWithIndex(f)(fa, lb)

    override def traverse[G[_], A, B](fa: Chain[A])(f: A => G[B])(implicit ev: Applicative[G]): G[Chain[B]] =
      catsDataInstancesForChain.traverse(fa)(f)

    override def exists[A](fa: Chain[A])(f: A => Boolean): Boolean = foldableWithIndexChain.exists(fa)(f)
  }

  implicit final val traverseWithIndexNonEmptyVector: TraverseWithIndex[NonEmptyVector, Int] = new TraverseWithIndex[NonEmptyVector, Int] {
    override def mapWithIndex[A, B](f: (A, Int) => B)(fa: NonEmptyVector[A]): NonEmptyVector[B] =
      functorWithIndexNonEmptyVector.mapWithIndex(f)(fa)

    override def foldLeftWithIndex[A, B](f: (B, (A, Int)) => B)(fa: NonEmptyVector[A], b: B): B =
      foldableWithIndexNonEmptyVector.foldLeftWithIndex(f)(fa, b)

    override def foldRightWithIndex[A, B](f: ((A, Int), Eval[B]) => Eval[B])(fa: NonEmptyVector[A], lb: Eval[B]): Eval[B] =
      foldableWithIndexNonEmptyVector.foldRightWithIndex(f)(fa, lb)

    override def traverse[G[_], A, B](fa: NonEmptyVector[A])(f: A => G[B])(implicit ev: Applicative[G]): G[NonEmptyVector[B]] =
      catsDataInstancesForNonEmptyVectorBinCompat1.traverse(fa)(f)

    override def exists[A](fa: NonEmptyVector[A])(f: A => Boolean): Boolean = foldableWithIndexNonEmptyVector.exists(fa)(f)
  }

  implicit final val traverseWithIndexNonEmptyList: TraverseWithIndex[NonEmptyList, Int] = new TraverseWithIndex[NonEmptyList, Int] {
    override def mapWithIndex[A, B](f: (A, Int) => B)(fa: NonEmptyList[A]): NonEmptyList[B] =
      functorWithIndexNonEmptyList.mapWithIndex(f)(fa)

    override def foldLeftWithIndex[A, B](f: (B, (A, Int)) => B)(fa: NonEmptyList[A], b: B): B =
      foldableWithIndexNonEmptyList.foldLeftWithIndex(f)(fa, b)

    override def foldRightWithIndex[A, B](f: ((A, Int), Eval[B]) => Eval[B])(fa: NonEmptyList[A], lb: Eval[B]): Eval[B] =
      foldableWithIndexNonEmptyList.foldRightWithIndex(f)(fa, lb)

    override def traverse[G[_], A, B](fa: NonEmptyList[A])(f: A => G[B])(implicit ev: Applicative[G]): G[NonEmptyList[B]] =
      fa.traverse(f)

    override def exists[A](fa: NonEmptyList[A])(f: A => Boolean): Boolean = foldableWithIndexNonEmptyList.exists(fa)(f)
  }

  implicit final def traverseWithIndexNonEmptyChain: TraverseWithIndex[NonEmptyChain, Int] = new TraverseWithIndex[NonEmptyChain, Int] {
    override def mapWithIndex[A, B](f: (A, Int) => B)(fa: NonEmptyChain[A]): NonEmptyChain[B] =
      functorWithIndexNonEmptyChain.mapWithIndex(f)(fa)

    override def foldLeftWithIndex[A, B](f: (B, (A, Int)) => B)(fa: NonEmptyChain[A], b: B): B =
      foldableWithIndexNonEmptyChain.foldLeftWithIndex(f)(fa, b)

    override def foldRightWithIndex[A, B](f: ((A, Int), Eval[B]) => Eval[B])(fa: NonEmptyChain[A], lb: Eval[B]): Eval[B] =
      foldableWithIndexNonEmptyChain.foldRightWithIndex(f)(fa, lb)

    override def traverse[G[_], A, B](fa: NonEmptyChain[A])(f: A => G[B])(implicit ev: Applicative[G]): G[NonEmptyChain[B]] =
      catsDataInstancesForNonEmptyChainBinCompat1.traverse(fa)(f)

    override def exists[A](fa: NonEmptyChain[A])(f: A => Boolean): Boolean = foldableWithIndexNonEmptyChain.exists(fa)(f)
  }

  implicit def traverseWithIndexOneAnd[F[_]: Traverse]: TraverseWithIndex[OneAnd[F, *], Int] = new TraverseWithIndex[OneAnd[F, *], Int] {
    override def mapWithIndex[A, B](f: (A, Int) => B)(fa: OneAnd[F, A]): OneAnd[F, B] =
      functorWithIndexOneAnd[F].mapWithIndex(f)(fa)

    override def foldLeftWithIndex[A, B](f: (B, (A, Int)) => B)(fa: OneAnd[F, A], b: B): B =
      foldableWithIndexOneAnd[F].foldLeftWithIndex(f)(fa, b)

    override def foldRightWithIndex[A, B](f: ((A, Int), Eval[B]) => Eval[B])(fa: OneAnd[F, A], lb: Eval[B]): Eval[B] =
      foldableWithIndexOneAnd[F].foldRightWithIndex(f)(fa, lb)

    override def traverse[G[_], A, B](fa: OneAnd[F, A])(f: A => G[B])(implicit ev: Applicative[G]): G[OneAnd[F, B]] =
      catsDataTraverseForOneAnd[F].traverse(fa)(f)

    override def exists[A](fa: OneAnd[F, A])(f: A => Boolean): Boolean = foldableWithIndexOneAnd[F].exists(fa)(f)
  }

  implicit def traverseWithIndexNonEmptyMap[K: Order]: TraverseWithIndex[NonEmptyMap[K, *], K] = new TraverseWithIndex[NonEmptyMap[K, *], K] {
    override def mapWithIndex[A, B](f: (A, K) => B)(fa: NonEmptyMap[K, A]): NonEmptyMap[K, B] =
      functorWithIndexNonEmptyMap[K].mapWithIndex[A, B](f)(fa)

    override def foldLeftWithIndex[A, B](f: (B, (A, K)) => B)(fa: NonEmptyMap[K, A], b: B): B =
      foldableWithIndexNonEmptyMap.foldLeftWithIndex(f)(fa, b)

    override def foldRightWithIndex[A, B](f: ((A, K), Eval[B]) => Eval[B])(fa: NonEmptyMap[K, A], lb: Eval[B]): Eval[B] =
      foldableWithIndexNonEmptyMap.foldRightWithIndex(f)(fa, lb)

    override def traverse[G[_], A, B](fa: NonEmptyMap[K, A])(f: A => G[B])(implicit ev: Applicative[G]): G[NonEmptyMap[K, B]] =
      fa.nonEmptyTraverse(f)

    override def exists[A](fa: NonEmptyMap[K, A])(f: A => Boolean): Boolean = foldableWithIndexNonEmptyMap.exists(fa)(f)
  }
}
