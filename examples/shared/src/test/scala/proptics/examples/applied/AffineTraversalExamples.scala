package proptics.examples.applied

import cats.syntax.option._

import proptics.instances.all._
import proptics.specs.PropticsSuite
import proptics.syntax.applied.all._

class AffineTraversalExamples extends PropticsSuite {
  test("preview the head of a list within a tuple") {
    val result = (9, List("head", "?", "?")).second.headOption

    assertResult("head".some)(result.preview)
  }

  test("transform each head of a nested list to upper case") {
    val target = List(List("a", "b", "c"), List("b", "c", "d"), Nil)
    val result = target.traversal.headOption.over(_.toUpperCase)

    assertResult(List(List("A", "b", "c"), List("B", "c", "d"), Nil))(result)
  }

  test("transform each tail of a nested list to upper case") {
    val target = List(List("a", "b", "c"), List("b", "c", "d"), Nil)
    val result = target.traversal.tailOption.over(_.map(_.toUpperCase))

    assertResult(List(List("a", "B", "C"), List("b", "C", "D"), Nil))(result)
  }
}
