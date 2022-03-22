package proptics.examples

import spire.std.int._

import proptics.instances.traverseWithIndex._
import proptics.specs.PropticsSuite
import proptics.syntax.indexedFold._
import proptics.syntax.indexedTraversal._
import proptics.{IndexedFold, IndexedTraversal}

trait IndexedFoldCompatSuite extends PropticsSuite {
  test("calculate total number of commits for a specific repo in the past week") {
    val fold =
      (IndexedFold.fromFoldableWithIndex[Map[String, *], String, Map[String, Int]] *>>
        IndexedFold.fromFoldableWithIndex[Map[String, *], String, Int])
        .index("repo A")

    assertResult(33)(fold.sum(commits))
  }

  test("calculate total number of commits for a specific repo in the past week") {
    val traversal =
      (IndexedTraversal.fromTraverseWithIndex[Map[String, *], String, Map[String, Int]] *>>
        IndexedTraversal.fromTraverseWithIndex[Map[String, *], String, Int])
        .single("repo A")

    assertResult(33)(traversal.sum(commits))
  }
}
