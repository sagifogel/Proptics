package proptics

import spire.std.int._
import proptics.specs.PropticsSuite

trait IndexedFoldCompatSuite extends PropticsSuite {
  test("calculate total number of commits for a specific repo in the past week") {
    val fold =
      (IndexedFold.fromFoldableWithIndex[Map[String, *], String, Map[String, Int]] *>>
        IndexedFold.fromFoldableWithIndex[Map[String, *], String, Int])
        .elementAt("repo A")

    assertResult(33)(fold.sum(commits))
  }

  test("calculate total number of commits for a specific repo in the past week") {
    val traversal =
      (IndexedTraversal.fromTraverseWithIndex[Map[String, *], String, Map[String, Int]] *>>
        IndexedTraversal.fromTraverseWithIndex[Map[String, *], String, Int])
        .elementAt("repo A")

    assertResult(33)(traversal.sum(commits))
  }

}
