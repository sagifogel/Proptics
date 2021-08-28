package proptics

import spire.std.int._
import proptics.specs.PropticsSuite

trait IndexedTraversalCompatSuite extends PropticsSuite {
  test("calculate total number of commits for a specific repo in the past week") {
    val traversal =
      (IndexedTraversal.fromTraverseWithIndex[Map[String, *], String, Map[String, Int]] *>>
        IndexedTraversal.fromTraverseWithIndex[Map[String, *], String, Int])
        .elementAt("repo A")

    assertResult(33)(traversal.sum(commits))
  }

}
