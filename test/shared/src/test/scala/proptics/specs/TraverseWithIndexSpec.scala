package proptics.specs

import cats.Id
import cats.data.{NonEmptyList, NonEmptyMap, NonEmptyVector}
import org.scalacheck.ScalacheckShapeless._

import proptics.instances.traverseWithIndex._
import proptics.law.discipline.TraverseWithIndexTests

class TraverseWithIndexSpec extends PropticsSuite {
  checkAll("TraverseWithIndex[Option, Unit]", TraverseWithIndexTests[Option, Unit].traverseWithIndex[Int, Int, Int, Int, Id, Id])
  checkAll("TraverseWithIndex[Vector, Int]", TraverseWithIndexTests[Vector, Int].traverseWithIndex[Int, Int, Int, Int, Id, Id])
  checkAll("TraverseWithIndex[List, Int]", TraverseWithIndexTests[List, Int].traverseWithIndex[Int, Int, Int, Int, Id, Id])
  checkAll("TraverseWithIndex[NonEmptyVector, Int]", TraverseWithIndexTests[NonEmptyVector, Int].traverseWithIndex[Int, Int, Int, Int, Id, Id])
  checkAll("TraverseWithIndex[NonEmptyList, Int]", TraverseWithIndexTests[NonEmptyList, Int].traverseWithIndex[Int, Int, Int, Int, Id, Id])
  checkAll("TraverseWithIndex[NonEmptyMap[Int, *], Int]", TraverseWithIndexTests[NonEmptyMap[Int, *], Int].traverseWithIndex[Int, Int, Int, Int, Id, Id])
}
