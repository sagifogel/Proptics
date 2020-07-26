package proptics.specs

import cats.Id
import cats.data.NonEmptyList
import cats.instances.int._
import cats.instances.option._
import cats.syntax.option._
import proptics.IndexedLens
import proptics.law.{IndexedLensRules, LensRules}

class IndexedLensSpec extends PropticsSuite {
  val wholeIndexedLens: IndexedLens[Int, Whole, Int] =
    IndexedLens[Int, Whole, Int](w => (w.part, w.part))(w => p => w.copy(part = p))

  val indexedLens: IndexedLens[Int, NonEmptyList[Int], Int] =
    IndexedLens[Int, NonEmptyList[Int], Int](ls => (0, ls.head))(nel => i => nel.copy(head = i))

  checkAll("IndexedLens apply", IndexedLensRules(indexedLens))
  checkAll("IndexedLens unindex", LensRules(wholeIndexedLens.unindex))

  test("view") {
    indexedLens.view(nel) shouldEqual ((0, 1))
  }

  test("set") {
    indexedLens.set(9)(nel) shouldEqual nel.copy(head = 9)
  }

  test("over") {
    indexedLens.over(oneToNine)(nel) shouldEqual nel.copy(head = 9)
  }

  test("traverse") {
    val result = indexedLens.traverse[Id](nel)(oneToNine)

    result shouldEqual nel.copy(head = 9)
    indexedLens.overF[Id](oneToNine)(nel) shouldEqual result
  }

  test("exists") {
    indexedLens.exists(_ === ((0, 1)))(nel)
  }

  test("notExists") {
    indexedLens.notExists(_ === ((0, 1)))(nel) shouldEqual false
    indexedLens.notExists(_ === ((1, 1)))(nel) shouldEqual true
    indexedLens.notExists(_ === ((0, 2)))(nel) shouldEqual true
    indexedLens.notExists(_ === ((0, 1)))(nel) shouldEqual !indexedLens.exists(_ == ((0, 1)))(nel)
  }

  test("contains") {
    indexedLens.contains(nel)((0, 1)) shouldEqual true
    indexedLens.contains(nel)((1, 1)) shouldEqual false
  }

  test("notContains") {
    indexedLens.notContains(nel)((0, 1)) shouldEqual false
    indexedLens.notContains(nel)((1, 1)) shouldEqual true
    indexedLens.notContains(nel)((1, 1)) shouldEqual !indexedLens.contains(nel)((1, 1))
  }

  test("find") {
    indexedLens.find { case (i, a) => i === 0 && a === 1 }(nel) shouldEqual 1.some
    indexedLens.find(_ === ((0, 1)))(nel) shouldEqual 1.some
    indexedLens.find(_._2 === 0)(nel) shouldEqual None
    indexedLens.find(_ === ((1, 1)))(nel) shouldEqual None
  }

  test("use") {
    indexedLens.use.runA(nel).value shouldEqual ((0, 1))
  }

  test("failover") {
    val res = indexedLens.failover[Option](nel)(_._2)(strongStarTupleOfDisj, catsStdInstancesForOption)
    val negativeRes = indexedLens.failover[Option](nel)(_._2)(strongStarTupleOfNegativeDisj, catsStdInstancesForOption)

    res shouldEqual Some(nel)
    negativeRes shouldEqual None
  }

  test("zipWith") {
    val secondNel = NonEmptyList.fromListUnsafe(List(8, 9, 10))
    val result = NonEmptyList.fromListUnsafe(List(9, 2, 3))

    indexedLens.zipWith(nel, secondNel) { case ((_, a1), (_, a2)) => a1 + a2 } shouldEqual result
  }

  test("cotraverse") {
    val cotraversedNel = indexedLens.cotraverse[Id](nel)(_._2)

    cotraversedNel shouldEqual nel
    indexedLens.zipWithF[Id](_._2)(nel) shouldEqual cotraversedNel
  }
}
