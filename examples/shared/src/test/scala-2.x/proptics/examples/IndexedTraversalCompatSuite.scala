package proptics.examples

import spire.std.int._

import proptics.IndexedTraversal
import proptics.instances.traverseWithIndex._
import proptics.specs.PropticsSuite
import proptics.syntax.indexedTraversal._

trait IndexedTraversalCompatSuite extends PropticsSuite {
  test("calculate total number of commits for a specific repo in the past week") {
    val traversal =
      (IndexedTraversal.fromTraverseWithIndex[Map[String, *], String, Map[String, Int]] *>>
        IndexedTraversal.fromTraverseWithIndex[Map[String, *], String, Int])
        .single("repo A")

    assertResult(33)(traversal.sum(commits))
  }
}
