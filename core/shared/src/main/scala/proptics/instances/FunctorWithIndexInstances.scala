package proptics.instances

import scala.collection.immutable.ListMap

import cats.data.OneAnd._
import cats.data._
import cats.instances.list._
import cats.{Order, Traverse}

import proptics.indices.FunctorWithIndex

trait FunctorWithIndexInstances extends ScalaVersionSpecificFunctorWithIndexInstances {
  implicit val functorWithIndexOption: FunctorWithIndex[Option, Unit] = new FunctorWithIndex[Option, Unit] {
    override def mapWithIndex[A, B](f: (A, Unit) => B)(fa: Option[A]): Option[B] = fa.map(f(_, ()))
  }

  implicit val functorWithIndexVector: FunctorWithIndex[Vector, Int] = new FunctorWithIndex[Vector, Int] {
    override def mapWithIndex[A, B](f: (A, Int) => B)(fa: Vector[A]): Vector[B] =
      Traverse[Vector].mapWithIndex(fa)(f)
  }

  implicit val functorWithIndexList: FunctorWithIndex[List, Int] = new FunctorWithIndex[List, Int] {
    override def mapWithIndex[A, B](f: (A, Int) => B)(fa: List[A]): List[B] =
      Traverse[List].mapWithIndex(fa)(f)
  }

  implicit def functorWithIndexListMap[K]: FunctorWithIndex[ListMap[K, *], K] = new FunctorWithIndex[ListMap[K, *], K] {
    override def mapWithIndex[A, B](f: (A, K) => B)(fa: ListMap[K, A]): ListMap[K, B] =
      fa.map { case (k, a) => (k, f(a, k)) }
  }

  implicit def functorWithIndexMap[K]: FunctorWithIndex[Map[K, *], K] = new FunctorWithIndex[Map[K, *], K] {
    override def mapWithIndex[A, B](f: (A, K) => B)(fa: Map[K, A]): Map[K, B] =
      fa.map { case (key, value) => (key, f(value, key)) }
  }

  implicit val functorWithIndexNev: FunctorWithIndex[NonEmptyVector, Int] = new FunctorWithIndex[NonEmptyVector, Int] {
    override def mapWithIndex[A, B](f: (A, Int) => B)(fa: NonEmptyVector[A]): NonEmptyVector[B] =
      Traverse[NonEmptyVector].mapWithIndex(fa)(f)
  }

  implicit val functorWithIndexNel: FunctorWithIndex[NonEmptyList, Int] = new FunctorWithIndex[NonEmptyList, Int] {
    override def mapWithIndex[A, B](f: (A, Int) => B)(fa: NonEmptyList[A]): NonEmptyList[B] =
      Traverse[NonEmptyList].mapWithIndex(fa)(f)
  }

  implicit def functorWithIndexNem[K: Order]: FunctorWithIndex[NonEmptyMap[K, *], K] = new FunctorWithIndex[NonEmptyMap[K, *], K] {
    override def mapWithIndex[A, B](f: (A, K) => B)(fa: NonEmptyMap[K, A]): NonEmptyMap[K, B] = {
      val mappedWithIndex = fa.toNel.map { case (k, a) => (k, f(a, k)) }

      NonEmptyMap.of(mappedWithIndex.head, mappedWithIndex.tail: _*)
    }
  }

  implicit val functorWithIndexChain: FunctorWithIndex[Chain, Int] = new FunctorWithIndex[Chain, Int] {
    override def mapWithIndex[A, B](f: (A, Int) => B)(fa: Chain[A]): Chain[B] =
      Traverse[Chain].mapWithIndex(fa)(f)
  }

  implicit val functorWithIndexNec: FunctorWithIndex[NonEmptyChain, Int] = new FunctorWithIndex[NonEmptyChain, Int] {
    override def mapWithIndex[A, B](f: (A, Int) => B)(fa: NonEmptyChain[A]): NonEmptyChain[B] =
      Traverse[NonEmptyChain].mapWithIndex(fa)(f)
  }

  implicit def functorWithIndexOneAnd[F[_]: Traverse]: FunctorWithIndex[OneAnd[F, *], Int] = new FunctorWithIndex[OneAnd[F, *], Int] {
    override def mapWithIndex[A, B](f: (A, Int) => B)(fa: OneAnd[F, A]): OneAnd[F, B] =
      Traverse[OneAnd[F, *]].mapWithIndex(fa)(f)
  }
}
