package proptics.instances

import cats.Traverse
import cats.data.NonEmptyList
import cats.instances.list._

import proptics.indices.FunctorWithIndex

trait FunctorWithIndexInstances {
  implicit val functorWithIndexOption: FunctorWithIndex[Option, Unit] = new FunctorWithIndex[Option, Unit] {
    override def mapWithIndex[A, B](f: (A, Unit) => B)(fa: Option[A]): Option[B] = fa.map(f(_, ()))
  }

  implicit val functorWithIndexList: FunctorWithIndex[List, Int] = new FunctorWithIndex[List, Int] {
    override def mapWithIndex[A, B](f: (A, Int) => B)(fa: List[A]): List[B] =
      Traverse[List].mapWithIndex(fa)(f)
  }

  implicit val functorWithIndexNel: FunctorWithIndex[NonEmptyList, Int] = new FunctorWithIndex[NonEmptyList, Int] {
    override def mapWithIndex[A, B](f: (A, Int) => B)(fa: NonEmptyList[A]): NonEmptyList[B] =
      Traverse[NonEmptyList].mapWithIndex(fa)(f)
  }

  implicit def functorWithIndexMap[K]: FunctorWithIndex[Map[K, *], K] = new FunctorWithIndex[Map[K, *], K] {
    override def mapWithIndex[A, B](f: (A, K) => B)(fa: Map[K, A]): Map[K, B] =
      fa.map { case (key, value) => (key, f(value, key)) }
  }
}
