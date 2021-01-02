package optics

import cats.Id
import cats.syntax.option._

import proptics.IndexedTraversal
import proptics.instances.traversalWithIndex._
import proptics.specs.PropticsSuite
import proptics.syntax.indexedTraversal._

class IndexedTraversalExamples extends PropticsSuite {
  val even: Int => Boolean = _ % 2 === 0

  def withIndex[A](list: List[A]): List[(Int, A)] = list.zipWithIndex.map(_.swap)

  test("use `fromIndexableTraverse` to manage indices internally") {
    val traversal = IndexedTraversal.fromIndexableTraverse[List, Int]
    val traversed = traversal.traverse[Id](List(10, 20, 30)) { case (i, j) => i + j }

    assertResult(List(10, 21, 32))(traversed)
  }

  test("get a list of all elements and their key") {
    val traversal = IndexedTraversal.fromTraverse[List, Int, Int]
    val traversed = traversal.viewAll(List(1, 2, 3))

    assertResult(List((1, 0), (2, 1), (3, 2)))(traversed)
  }

  test("filter only characters which are in an even index position") {
    val traversal = IndexedTraversal.fromTraverse[List, Int, Char].filterByIndex(even)
    val traversed = traversal.toList(('a' to 'z').toList)

    assertResult("acegikmoqsuwy")(traversed.mkString)
  }

  test("focus on an element at a specific index") {
    val traversal = IndexedTraversal.fromTraverse[List, Int, Char].element(10)

    assertResult('k'.some)(traversal.preview(('a' to 'z').toList))
  }

  test("focus on elements at an even index and are greater than 5") {
    val traversal = IndexedTraversal.fromTraverse[List, Int, Int].filter { case (a, i) => even(i) & a > 5 }

    assertResult(List((7, 6), (9, 8)))(traversal.viewAll(List.range(1, 10)))
  }

  test("map over the indexes of an optic") {
    val input = List("A", "B", "C")
    val expected = List(("A", 0), ("B", 10), ("C", 20))
    val traversal =
      IndexedTraversal
        .fromTraverse[List, Int, String]
        .reindex(_ * 10)

    assertResult(expected)(traversal.viewAll(input))
  }

  test("unindex an indexed traversal") {
    val input = List("A", "B", "C")
    val traversal =
      IndexedTraversal
        .fromTraverse[List, Int, String]
        .unIndex

    assertResult(input)(traversal.viewAll(input))
  }
}
