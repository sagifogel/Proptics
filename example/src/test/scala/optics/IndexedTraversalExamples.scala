package optics

import cats.Id
import cats.syntax.option._

import proptics.instances.traverseWithIndex._
import proptics.specs.PropticsSuite
import proptics.syntax.indexedTraversal._
import proptics.syntax.tuple._
import proptics.{IndexedTraversal, IndexedTraversal_}

class IndexedTraversalExamples extends PropticsSuite {
  val even: Int => Boolean = _ % 2 === 0

  test("use `fromTraverse` for Traversal with Int indices") {
    val traversal = IndexedTraversal.fromTraverse[List, Int]
    val traversed = traversal.traverse[Id](List(10, 20, 30)) { case (i, j) => i + j }

    assertResult(List(10, 21, 32))(traversed)
  }

  test("get a list of all elements and their key") {
    val traversal = IndexedTraversal.fromTraverseWithIndex[List, Int, Int]
    val traversed = traversal.viewAll(List(1, 2, 3))

    assertResult(List((1, 0), (2, 1), (3, 2)))(traversed)
  }

  test("filter only characters which are in an even index position") {
    val traversal = IndexedTraversal.fromTraverseWithIndex[List, Int, Char].filterByIndex(even)
    val traversed = traversal.toList(('a' to 'z').toList)

    assertResult("acegikmoqsuwy")(traversed.mkString)
  }

  test("focus on an element at a specific index") {
    val traversal = IndexedTraversal.fromTraverseWithIndex[List, Int, Char].element(10)

    assertResult('k'.some)(traversal.preview(('a' to 'z').toList))
  }

  test("focus on elements that reside at an even index and are greater than 5") {
    val traversal = IndexedTraversal.fromTraverseWithIndex[List, Int, Int].filter { case (a, i) => even(i) & a > 5 }

    assertResult(List((7, 6), (9, 8)))(traversal.viewAll(List.range(1, 10)))
  }

  test("map over the indices of an optic") {
    val input = List("A", "B", "C")
    val expected = List(("A", 0), ("B", 10), ("C", 20))
    val traversal =
      IndexedTraversal
        .fromTraverseWithIndex[List, Int, String]
        .reindex(_ * 10)

    assertResult(expected)(traversal.viewAll(input))
  }

  test("unindex an IndexedTraversal") {
    val input = List("A", "B", "C")
    val traversal =
      IndexedTraversal
        .fromTraverseWithIndex[List, Int, String]
        .unIndex

    assertResult(input)(traversal.viewAll(input))
  }

  test("summon and use an instance of TraverseWithIndex for a Map") {
    val firstMap = Map[Int, String](0 -> "0", 1 -> "1", 2 -> "2")
    val secondMap = Map[Int, String](0 -> "A", 1 -> "1", 2 -> "2")
    val traversal = IndexedTraversal_.fromTraverseWithIndex[Map[Int, *], Int, String, Int]
    val partialTraverse = traversal.traverse[Option](_: Map[Int, String])(parseInt _ compose Tuple2._1)

    assertResult(Map(0 -> 0, 1 -> 1, 2 -> 2).some)(partialTraverse(firstMap))
    assertResult(None)(partialTraverse(secondMap))
  }

  test("compose with other optic and taking the right optic's indices") {
    val map = List(("Scala", List("Some", "None")), ("Haskell", List("Just", "Nothing"))).toMap
    val mapTraversal = IndexedTraversal.fromTraverseWithIndex[Map[String, *], String, List[String]]
    val listTraversal = IndexedTraversal.fromTraverseWithIndex[List, Int, String]
    val composedWithRightIndex: IndexedTraversal[Int, Map[String, List[String]], String] =
      mapTraversal *> listTraversal
    val expected = List(("Some", 0), ("None", 1), ("Just", 0), ("Nothing", 1))

    assertResult(expected)(composedWithRightIndex.viewAll(map))
  }

  test("compose with other optic and taking self indices") {
    val map = List(("Scala", List("Some", "None")), ("Haskell", List("Just", "Nothing"))).toMap
    val mapTraversal = IndexedTraversal.fromTraverseWithIndex[Map[String, *], String, List[String]]
    val listTraversal = IndexedTraversal.fromTraverseWithIndex[List, Int, String]
    val composedWithLeftIndex: IndexedTraversal[String, Map[String, List[String]], String] =
      mapTraversal <* listTraversal
    val expected = List(("Some", "Scala"), ("None", "Scala"), ("Just", "Haskell"), ("Nothing", "Haskell"))

    assertResult(expected)(composedWithLeftIndex.viewAll(map))
  }
}
