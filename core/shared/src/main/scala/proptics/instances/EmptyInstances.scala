package proptics.instances

import scala.collection.immutable.{ListMap, SortedMap}
import scala.reflect.ClassTag

import cats.Order
import cats.data.Chain

import proptics.Prism
import proptics.typeclass.Empty

trait EmptyInstances extends ScalaVersionSpecificEmptyInstances {
  def empty[S](implicit ev: Empty[S]): Prism[S, Unit] = ev.empty

  implicit final val emptyString: Empty[String] = new Empty[String] {
    override def empty: Prism[String, Unit] = Prism.nearly("")(_.isEmpty)
  }

  implicit final def emptyArray[A: ClassTag]: Empty[Array[A]] = new Empty[Array[A]] {
    override def empty: Prism[Array[A], Unit] = Prism.nearly(Array.empty[A])(_.isEmpty)
  }

  implicit final def emptyList[A]: Empty[List[A]] = new Empty[List[A]] {
    override def empty: Prism[List[A], Unit] = Prism.nearly(List.empty[A])(_.isEmpty)
  }

  implicit final def emptyVector[A]: Empty[Vector[A]] = new Empty[Vector[A]] {
    override def empty: Prism[Vector[A], Unit] = Prism.nearly(Vector.empty[A])(_.isEmpty)
  }

  implicit final def emptySet[A]: Empty[Set[A]] = new Empty[Set[A]] {
    override def empty: Prism[Set[A], Unit] = Prism.nearly(Set.empty[A])(_.isEmpty)
  }

  implicit final def emptyListMap[K, V]: Empty[ListMap[K, V]] = new Empty[ListMap[K, V]] {
    override def empty: Prism[ListMap[K, V], Unit] = Prism.nearly(ListMap.empty[K, V])(_.isEmpty)
  }

  implicit final def emptyMap[K, V]: Empty[Map[K, V]] = new Empty[Map[K, V]] {
    override def empty: Prism[Map[K, V], Unit] = Prism.nearly(Map.empty[K, V])(_.isEmpty)
  }

  implicit final def emptySortedMap[K, V](implicit ev: Order[K]): Empty[SortedMap[K, V]] = new Empty[SortedMap[K, V]] {
    override def empty: Prism[SortedMap[K, V], Unit] = Prism.nearly(SortedMap.empty[K, V](ev.toOrdering))(_.isEmpty)
  }

  implicit final def emptyChain[A]: Empty[Chain[A]] = new Empty[Chain[A]] {
    override def empty: Prism[Chain[A], Unit] = Prism.nearly(Chain.empty[A])(_.isEmpty)
  }

}
