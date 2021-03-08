package proptics.specs

import cats.Id

import proptics.instances.traverseWithIndex._
import proptics.law.discipline.TraverseWithIndexTests

class TraverseWithIndexSpec extends TraverseWithIndexSpec0 {
  checkAll("TraverseWithIndex[Stream, Int]", TraverseWithIndexTests[Stream, Int].traverseWithIndex[Int, Int, Int, Int, Id, Id])
}
