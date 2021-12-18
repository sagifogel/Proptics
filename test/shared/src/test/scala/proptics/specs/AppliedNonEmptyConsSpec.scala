package proptics.specs
import cats.data.NonEmptyList

import proptics.instances.field2._
import proptics.instances.nonEmptyCons._
import proptics.syntax.applied.all._

class AppliedNonEmptyConsSpec extends PropticsSuite {
  val target: (Int, NonEmptyList[String]) =
    (9, NonEmptyList.fromListUnsafe(List("head", "?", "?")))

  test("view the head and the tail of a NonEmptyList within a tuple") {
    val result = target.second.nonEmptyCons.view

    assertResult(("head", List("?", "?")))(result)
  }

  test("view the head of a NonEmptyList within a tuple") {
    val result = target.second.head.view

    assertResult("head")(result)
  }

  test("view the tail of a NonEmptyList within a tuple") {
    val result = target.second.tail.view

    assertResult(List("?", "?"))(result)
  }
}
