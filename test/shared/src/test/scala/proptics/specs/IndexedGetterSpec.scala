package proptics.specs

import cats.syntax.option._
import cats.data.NonEmptyList
import proptics.IndexedGetter

class IndexedGetterSpec extends PropticsSuite {
  val indexedGetter: IndexedGetter[Int, NonEmptyList[Int], Int] =
    IndexedGetter[Int, NonEmptyList[Int], Int](nel => (0, nel.head))

  test("view") {
    indexedGetter.view(nel) shouldEqual ((0, 1))
  }

  test("exists") {
    indexedGetter.exists(_ === ((0, 1)))(nel) shouldEqual true
  }

  test("notExists") {
    indexedGetter.notExists(_ === ((0, 1)))(nel) shouldEqual false
    indexedGetter.notExists(_ === ((1, 1)))(nel) shouldEqual true
    indexedGetter.notExists(_ === ((0, 2)))(nel) shouldEqual true
    indexedGetter.notExists(_ === ((0, 1)))(nel) shouldEqual !indexedGetter.exists(_ == ((0, 1)))(nel)
  }

  test("contains") {
    indexedGetter.contains(nel)((0, 1)) shouldEqual true
    indexedGetter.contains(nel)((1, 1)) shouldEqual false
  }

  test("notContains") {
    indexedGetter.notContains(nel)((0, 1)) shouldEqual false
    indexedGetter.notContains(nel)((1, 1)) shouldEqual true
    indexedGetter.notContains(nel)((1, 1)) shouldEqual !indexedGetter.contains(nel)((1, 1))
  }

  test("find") {
    indexedGetter.find { case (i, a) => i === 0 && a === 1 }(nel) shouldEqual 1.some
    indexedGetter.find(_ === ((0, 1)))(nel) shouldEqual 1.some
    indexedGetter.find(_._2 === 0)(nel) shouldEqual None
    indexedGetter.find(_ === ((1, 1)))(nel) shouldEqual None
  }

  test("asGetter") {
    indexedGetter.asGetter.view(nel) shouldEqual 1
  }

  test("asIndexedFold") {
    indexedGetter.asIndexedFold.preview(nel) shouldEqual (0, 1).some
  }
}
