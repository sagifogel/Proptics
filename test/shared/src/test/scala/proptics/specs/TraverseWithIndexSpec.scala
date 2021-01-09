package proptics.specs

import cats.Id
import cats.data.NonEmptyList
import org.scalacheck.ScalacheckShapeless._

import proptics.instances.traverseWithIndex._
import proptics.law.discipline.TraverseWithIndexTests

class TraverseWithIndexSpec extends PropticsSuite {
  checkAll("TraverseWithIndex[List, Int]", TraverseWithIndexTests[List, Int].traverseWithIndex[Int, Int, Int, Int, Id, Id])
  checkAll("TraverseWithIndex[NonEmptyList, Int]", TraverseWithIndexTests[NonEmptyList, Int].traverseWithIndex[Int, Int, Int, Int, Id, Id])
  checkAll("TraverseWithIndex[Map[Int, *], Int]", TraverseWithIndexTests[Map[Int, *], Int].traverseWithIndex[Int, Int, Int, Int, Id, Id])
}
