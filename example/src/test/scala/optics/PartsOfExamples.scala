package optics

import proptics.instances.field1._
import proptics.instances.field2._
import proptics.instances.partsOf._
import proptics.specs.PropticsSuite
import proptics.std.tuple.{_1, _2}
import proptics.syntax.aTraversal._
import proptics.syntax.traversal._
import proptics.{ATraversal, Lens, Traversal}

class PartsOfExamples extends PropticsSuite {
  val traversedTuple1: Traversal[List[(String, Int)], String] = Traversal.fromTraverse[List, (String, Int)] compose _1[String, Int]
  val aTraversedTuple1: ATraversal[List[(String, Int)], String] = ATraversal.fromTraverse[List, (String, Int)] compose _1[String, Int]
  val traversedTuple2: Traversal[List[(String, Double)], Double] = Traversal.fromTraverse[List, (String, Double)] compose _2[String, Double]
  val aTraversedTuple2: ATraversal[List[(String, Double)], Double] = ATraversal.fromTraverse[List, (String, Double)] compose _2[String, Double]
  val partsOfFromTraversal1: Lens[List[(String, Int)], List[String]] = traversedTuple1.partsOf
  val partsOfFromATraversal1: Lens[List[(String, Int)], List[String]] = aTraversedTuple1.partsOf
  val partsOfFromTraversal2: Lens[List[(String, Double)], List[Double]] = traversedTuple2.partsOf
  val partsOfFromATraversal2: Lens[List[(String, Double)], List[Double]] = aTraversedTuple2.partsOf

  test("partsOf from Traversal can view focuses as List") {
    val target = List("A", "B", "C").zipWithIndex
    partsOfFromTraversal1.view(target) shouldEqual List("A", "B", "C")
  }

  test("partsOf from ATraversal can view focuses as List") {
    val target = List("A", "B", "C").zipWithIndex
    partsOfFromATraversal1.view(target) shouldEqual List("A", "B", "C")
  }

  test("partsOf from Traversal can set the lens to a list to replace the corresponding elements") {
    val target = List("A", "B", "C").zipWithIndex
    partsOfFromTraversal1.set(List("C", "A", "T"))(target) shouldEqual List(("C", 0), ("A", 1), ("T", 2))
  }

  test("partsOf from ATraversal can set the lens to a list to replace the corresponding elements") {
    val target = List("A", "B", "C").zipWithIndex
    partsOfFromATraversal1.set(List("C", "A", "T"))(target) shouldEqual List(("C", 0), ("A", 1), ("T", 2))
  }

  test("partsOf from Traversal ignores extra elements") {
    val target = List("A", "B", "C").zipWithIndex
    val replaceList = List("C", "A", "T", "E", "G", "O", "R", "Y")
    partsOfFromTraversal1.set(replaceList)(target) shouldEqual List(("C", 0), ("A", 1), ("T", 2))
  }

  test("partsOf from ATraversal ignores extra elements") {
    val target = List("A", "B", "C").zipWithIndex
    val replaceList = List("C", "A", "T", "E", "G", "O", "R", "Y")
    partsOfFromATraversal1.set(replaceList)(target) shouldEqual List(("C", 0), ("A", 1), ("T", 2))
  }

  test("partsOf from Traversal keeps the originals in case provided with few elements") {
    val target = List("A", "B", "C").zipWithIndex
    val replaceList = List("C")
    partsOfFromTraversal1.set(replaceList)(target) shouldEqual List(("C", 0), ("B", 1), ("C", 2))
  }

  test("partsOf from ATraversal keeps the originals in case provided with few elements") {
    val target = List("A", "B", "C").zipWithIndex
    val replaceList = List("C")
    partsOfFromATraversal1.set(replaceList)(target) shouldEqual List(("C", 0), ("B", 1), ("C", 2))
  }

  test("partsOf from Traversal can edit elements using their neighbours as context") {
    val target = List("A", "B", "C").zipWithIndex map { case (s, i) => (s, i + 1 / 1.0) }
    partsOfFromTraversal2.over { ls =>
      val sum = ls.sum
      ls.map(_ / sum)
    }(target) shouldEqual List(("A", 0.16666666666666666), ("B", 0.3333333333333333), ("C", 0.5))
  }

  test("partsOf from ATraversal can edit elements using their neighbours as context") {
    val target = List("A", "B", "C").zipWithIndex map { case (s, i) => (s, i + 1 / 1.0) }
    partsOfFromATraversal2.over { ls =>
      val sum = ls.sum
      ls.map(_ / sum)
    }(target) shouldEqual List(("A", 0.16666666666666666), ("B", 0.3333333333333333), ("C", 0.5))
  }

  test("partsOf from Traversal can reverse only the first 3 elements") {
    val partsOfTraversal = Traversal.fromTraverse[List, Int].take(3).partsOf
    val result = partsOfTraversal.over(_.reverse)(List(1, 2, 3, 4, 5, 6))

    result shouldEqual List(3, 2, 1, 4, 5, 6)
  }

  test("partsOf from ATraversal can reverse only the first 3 elements") {
    val partsOfATraversal = ATraversal.fromTraverse[List, Int].take(3).partsOf
    val result = partsOfATraversal.over(_.reverse)(List(1, 2, 3, 4, 5, 6))

    result shouldEqual List(3, 2, 1, 4, 5, 6)
  }
}
