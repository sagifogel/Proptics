package proptics.specs
import cats.syntax.option._
import cats.data.NonEmptyList
import proptics.IndexedGetter
import proptics.specs.compose._

class IndexedGetterSpec extends PropticsSuite {
  val nelIndexedGetter: IndexedGetter[Int, NonEmptyList[Int], Int] =
    IndexedGetter[Int, NonEmptyList[Int], Int](nel => (0, nel.head))

  test("view") {
    nelIndexedGetter.view(nel) shouldEqual ((0, 1))
  }

  test("exists") {
    nelIndexedGetter.exists(_ === ((0, 1)))(nel) shouldEqual true
  }

  test("notExists") {
    nelIndexedGetter.notExists(_ === ((0, 1)))(nel) shouldEqual false
    nelIndexedGetter.notExists(_ === ((1, 1)))(nel) shouldEqual true
    nelIndexedGetter.notExists(_ === ((0, 2)))(nel) shouldEqual true
    nelIndexedGetter.notExists(_ === ((0, 1)))(nel) shouldEqual !nelIndexedGetter.exists(_ == ((0, 1)))(nel)
  }

  test("contains") {
    nelIndexedGetter.contains(nel)((0, 1)) shouldEqual true
    nelIndexedGetter.contains(nel)((1, 1)) shouldEqual false
  }

  test("notContains") {
    nelIndexedGetter.notContains(nel)((0, 1)) shouldEqual false
    nelIndexedGetter.notContains(nel)((1, 1)) shouldEqual true
    nelIndexedGetter.notContains(nel)((1, 1)) shouldEqual !nelIndexedGetter.contains(nel)((1, 1))
  }

  test("find") {
    nelIndexedGetter.find { case (i, a) => i === 0 && a === 1 }(nel) shouldEqual 1.some
    nelIndexedGetter.find(_ === ((0, 1)))(nel) shouldEqual 1.some
    nelIndexedGetter.find(_._2 === 0)(nel) shouldEqual None
    nelIndexedGetter.find(_ === ((1, 1)))(nel) shouldEqual None
  }

  test("asGetter") {
    nelIndexedGetter.asGetter.view(nel) shouldEqual 1
  }

  test("asIndexedFold") {
    nelIndexedGetter.asIndexedFold.preview(nel) shouldEqual (0, 1).some
  }

  test("compose with IndexedLens") {
    (indexedGetter compose indexedLens).view(9) shouldEqual ((0, 9))
  }
  test("compose with AnIndexedLens") {
    (indexedGetter compose anIndexedLens).view(9) shouldEqual ((0, 9))
  }

  test("compose with IndexedTraversal") {
    (indexedGetter compose indexedTraversal).foldMap(9)(_._2) shouldEqual 9
  }

  test("compose with IndexedGetter") {
    (indexedGetter compose indexedGetter).view(9) shouldEqual ((0, 9))
  }

  test("compose with IndexedFold") {
    (indexedGetter compose indexedFold).foldMap(9)(_._2) shouldEqual 9
  }
}
