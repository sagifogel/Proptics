package optics

import cats.syntax.option._

import proptics.instances.cons._
import proptics.instances.field1._
import proptics.instances.field2._
import proptics.instances.prefixed._
import proptics.instances.suffixed._
import proptics.specs.PropticsSuite
import proptics.std.tuple._
import proptics.{AffineTraversal, Traversal}

class AffineTraversalExamples extends PropticsSuite {
  test("preview the head of a list within a tuple") {
    val composed = _2[Int, List[String]] andThen headOption[List[String], String]
    val result = composed.preview((9, List("head", "?", "?")))

    assertResult("head".some)(result)
  }

  test("transform each head of a nested list to upper case") {
    val composed = Traversal.fromTraverse[List, List[String]] andThen headOption[List[String], String]
    val result = composed.over(_.toUpperCase)(List(List("a", "b", "c"), List("b", "c", "d"), List.empty))

    assertResult(List(List("A", "b", "c"), List("B", "c", "d"), List.empty))(result)
  }

  test("remove the suffix or prefix of a string") {
    val suffixedComposed: AffineTraversal[(String, Int), String] =
      _1[String, Int] andThen suffixed[String, String]("fixed")
    val prefixedComposed: AffineTraversal[(String, Int), String] =
      _1[String, Int] andThen prefixed[String, String]("pre")

    assertResult("suf".some)(suffixedComposed.preview(("suffixed", 9)))
    assertResult("pre".some)(suffixedComposed.preview(("prefixed", 9)))
    assertResult("fixed".some)(prefixedComposed.preview(("prefixed", 9)))
    assertResult(None)(prefixedComposed.preview(("suffixed", 9)))
  }
}
