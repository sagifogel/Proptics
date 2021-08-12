package optics

import proptics.instances.partsOf._
import proptics.specs.PropticsSuite
import proptics.std.tuple._1P
import proptics.{ATraversal_, Lens_, Traversal_}

class UnsafePartsOfExamples extends PropticsSuite {
  val traversedTuple1: Traversal_[List[(String, Int)], List[(Boolean, Int)], String, Boolean] =
    Traversal_.fromTraverse[List, (String, Int), (Boolean, Int)] andThen
      _1P[String, Boolean, Int]
  val traversedTuple2: ATraversal_[List[(String, Int)], List[(Boolean, Int)], String, Boolean] =
    ATraversal_.fromTraverse[List, (String, Int), (Boolean, Int)] andThen
      _1P[String, Boolean, Int]
  val unsafePartsOfFromTraversal: Lens_[List[(String, Int)], List[(Boolean, Int)], List[String], List[Boolean]] =
    traversedTuple1.unsafePartsOf
  val unsafePartsOfFromATraversal: Lens_[List[(String, Int)], List[(Boolean, Int)], List[String], List[Boolean]] =
    traversedTuple2.unsafePartsOf

  test("unsafePartsOf from Traversal can view focuses as List") {
    val target = List("A", "B", "C").zipWithIndex
    unsafePartsOfFromTraversal.view(target) shouldEqual List("A", "B", "C")
  }

  test("unsafePartsOf from ATraversal can view focuses as List") {
    val target = List("A", "B", "C").zipWithIndex
    unsafePartsOfFromATraversal.view(target) shouldEqual List("A", "B", "C")
  }

  test("unsafePartsOf from Traversal can set the lens to a list to replace the corresponding elements") {
    val target = List("A", "B", "C").zipWithIndex
    unsafePartsOfFromTraversal.set(List(true, false, true))(target) shouldEqual
      List((true, 0), (false, 1), (true, 2))
  }

  test("unsafePartsOf from ATraversal can set the lens to a list to replace the corresponding elements") {
    val target = List("A", "B", "C").zipWithIndex
    unsafePartsOfFromATraversal.set(List(true, false, true))(target) shouldEqual
      List((true, 0), (false, 1), (true, 2))
  }

  test("unsafePartsOf from Traversal ignores extra elements") {
    val target = List("A", "B", "C").zipWithIndex
    val replaceList = List(true, false, true, false, true, false, true)

    unsafePartsOfFromTraversal.set(replaceList)(target) shouldEqual
      List((true, 0), (false, 1), (true, 2))
  }

  test("unsafePartsOf from ATraversal ignores extra elements") {
    val target = List("A", "B", "C").zipWithIndex
    val replaceList = List(true, false, true, false, true, false, true)

    unsafePartsOfFromATraversal.set(replaceList)(target) shouldEqual
      List((true, 0), (false, 1), (true, 2))
  }

  test("unsafePartsOf from Traversal crashes, when setting the lens with a wrong number of list elements") {
    val target = List("A", "B", "C").zipWithIndex
    val thrown = intercept[IllegalArgumentException] {
      unsafePartsOfFromTraversal.set(List(true, false))(target)
    }

    thrown.getMessage shouldEqual "Not enough elements were supplied"
  }

  test("unsafePartsOf from ATraversal crashes, when setting the lens with a wrong number of list elements") {
    val target = List("A", "B", "C").zipWithIndex
    val thrown = intercept[IllegalArgumentException] {
      unsafePartsOfFromATraversal.set(List(true, false))(target)
    }

    thrown.getMessage shouldEqual "Not enough elements were supplied"
  }

  test("unsafePartsOf from Traversal is safer when using over") {
    val target = List("A", "B", "C").zipWithIndex
    val replaceList = List(true, false)
    val result = unsafePartsOfFromATraversal.over { list =>
      list.zipWithIndex.map { case (_, i) => replaceList.lift(i).getOrElse(true) }
    }

    result(target) shouldEqual List((true, 0), (false, 1), (true, 2))
  }
}
