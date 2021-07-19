package proptics.specs
import cats.data.NonEmptyList
import cats.syntax.option._

import proptics.specs.compose._
import proptics.{IndexedGetter, Iso}

class IndexedGetterSpec extends PropticsSuite {
  val nelIndexedGetter: IndexedGetter[Int, NonEmptyList[Int], Int] =
    IndexedGetter[Int, NonEmptyList[Int], Int](nel => (nel.head, 0))

  test("view") {
    nelIndexedGetter.view(nel) shouldEqual ((1, 0))
  }

  test("exists") {
    nelIndexedGetter.exists(_ === ((1, 0)))(nel) shouldEqual true
  }

  test("notExists") {
    nelIndexedGetter.notExists(_ === ((1, 0)))(nel) shouldEqual false
    nelIndexedGetter.notExists(_ === ((1, 1)))(nel) shouldEqual true
    nelIndexedGetter.notExists(_ === ((2, 0)))(nel) shouldEqual true
    nelIndexedGetter.notExists(_ === ((1, 0)))(nel) shouldEqual !nelIndexedGetter.exists(_ == ((1, 0)))(nel)
  }

  test("contains") {
    nelIndexedGetter.contains((1, 0))(nel) shouldEqual true
    nelIndexedGetter.contains((1, 1))(nel) shouldEqual false
  }

  test("notContains") {
    nelIndexedGetter.notContains((1, 0))(nel) shouldEqual false
    nelIndexedGetter.notContains((1, 1))(nel) shouldEqual true
    nelIndexedGetter.notContains((1, 1))(nel) shouldEqual !nelIndexedGetter.contains((1, 1))(nel)
  }

  test("find") {
    nelIndexedGetter.find { case (a, i) => i === 0 && a === 1 }(nel) shouldEqual (1, 0).some
    nelIndexedGetter.find(_ === ((1, 0)))(nel) shouldEqual (1, 0).some
    nelIndexedGetter.find(_._1 === 0)(nel) shouldEqual None
    nelIndexedGetter.find(_ === ((1, 1)))(nel) shouldEqual None
  }

  test("asGetter") {
    nelIndexedGetter.asGetter.view(nel) shouldEqual 1
  }

  test("reindex") {
    indexedGetter.reindex(_.toString).view(9) shouldEqual ((9, "0"))
  }

  test("asIndexedFold") {
    nelIndexedGetter.asIndexedFold.preview(nel) shouldEqual (1, 0).some
  }

  test("compose with Iso[Int, Int]") {
    val composed = Iso.id[Int] compose IndexedGetter[Int, Int, Int]((_, 1))

    composed.view(9) shouldEqual 9
  }

  test("andThen with Iso[Int, Int]") {
    val composed = Iso.id[Int] andThen IndexedGetter[Int, Int, Int]((_, 1))

    composed.view(9) shouldEqual 9
  }

  test("compose with IndexedLens with right index") {
    val composed = IndexedGetter[Int, Int, Int]((_, 1)) *>> indexedLens

    composed.view(9) shouldEqual ((9, 0))
  }

  test("compose with IndexedLens with left index") {
    val composed = IndexedGetter[Int, Int, Int]((_, 1)) <<* indexedLens

    composed.view(9) shouldEqual ((9, 1))
  }

  test("compose with AnIndexedLens with right index") {
    val composed = IndexedGetter[Int, Int, Int]((_, 1)) *>> indexedLens

    composed.view(9) shouldEqual ((9, 0))
  }

  test("compose with AnIndexedLens with left index") {
    val composed = IndexedGetter[Int, Int, Int]((_, 1)) <<* indexedLens

    composed.view(9) shouldEqual ((9, 1))
  }

  test("compose with IndexedTraversal with right index") {
    val composed = IndexedGetter[Int, Int, Int]((_, 1)) *>> indexedTraversal

    composed.viewAll(9) shouldEqual List((9, 0))
  }

  test("compose with IndexedTraversal with left index") {
    val composed = IndexedGetter[Int, Int, Int]((_, 1)) <<* indexedTraversal

    composed.viewAll(9) shouldEqual List((9, 1))
  }

  test("compose with IndexedGetter with right index") {
    val composed = IndexedGetter[Int, Int, Int]((_, 1)) *>> indexedGetter

    composed.view(9) shouldEqual ((9, 0))
  }

  test("compose with IndexedGetter with left index") {
    val composed = IndexedGetter[Int, Int, Int]((_, 1)) <<* indexedGetter

    composed.view(9) shouldEqual ((9, 1))
  }

  test("compose with IndexedFold with right index") {
    val composed = IndexedGetter[Int, Int, Int]((_, 1)) *>> indexedFold

    composed.viewAll(9) shouldEqual List((9, 0))
  }

  test("compose with IndexedFold with left index") {
    val composed = IndexedGetter[Int, Int, Int]((_, 1)) <<* indexedFold

    composed.viewAll(9) shouldEqual List((9, 1))
  }
}
