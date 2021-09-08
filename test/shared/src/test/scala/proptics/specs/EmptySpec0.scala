package proptics.specs

import scala.collection.immutable.{ListMap, SortedMap}

import cats.data.Chain

import proptics.instances.empty._
import proptics.instances.empty.{emptyList => el}
import proptics.law.discipline.EmptyTests

private[specs] trait EmptySpec0 extends PropticsSuite {
  checkAll("Empty[String] empty", EmptyTests[String].empty)
  checkAll("Empty[Array[Int] empty", EmptyTests[Array[Int]].empty)
  checkAll("Empty[List[Int] empty", EmptyTests[List[Int]].empty)
  checkAll("Empty[Vector[Int] empty", EmptyTests[Vector[Int]].empty)
  checkAll("Empty[Set[Int] empty", EmptyTests[Set[Int]].empty)
  checkAll("Empty[ListMap[Int, Int] empty", EmptyTests[ListMap[Int, Int]].empty)
  checkAll("Empty[Map[Int, Int] empty", EmptyTests[Map[Int, Int]].empty)
  checkAll("Empty[SortedMap[Int, Int] empty", EmptyTests[SortedMap[Int, Int]].empty)
  checkAll("Empty[Chain[Int] empty", EmptyTests[Chain[Int]].empty)
}
