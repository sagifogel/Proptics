package proptics.specs

import cats.Id
import cats.data.NonEmptyList
import cats.instances.int._
import cats.instances.option._
import cats.syntax.option._
import proptics.IndexedLens
import proptics.law._
import proptics.specs.Compose._

class IndexedLensSpec extends PropticsSuite {
  val wholeIndexedLens: IndexedLens[Int, Whole, Int] =
    IndexedLens[Int, Whole, Int](w => (w.part, w.part))(w => p => w.copy(part = p))

  val nelIndexedLens: IndexedLens[Int, NonEmptyList[Int], Int] =
    IndexedLens[Int, NonEmptyList[Int], Int](ls => (0, ls.head))(nel => i => nel.copy(head = i))

  checkAll("IndexedLens[Int, NonEmptyList[Int], Int] apply", IndexedLensTests(nelIndexedLens).indexedLens)
  checkAll("IndexedLens[Int, Whole, Int] asLens", LensTests(wholeIndexedLens.asLens).lens)
  checkAll("IndexedLens[Int, Int, Int] compose with IndexedLens", IndexedLensTests(indexedLens compose indexedLens).indexedLens)
  checkAll("IndexedLens[Int, Int, Int] compose with AnIndexedLens", AnIndexedLensRules(indexedLens compose anIndexedLens))
  checkAll("IndexedLens[Int, Int, Int] compose with IndexedTraversal", IndexedTraversalRules(indexedLens compose indexedTraversal))
  checkAll("IndexedLens[Int, Int, Int] compose with IndexedSetter", IndexedSetterRules(indexedLens compose indexedSetter))

  test("view") {
    nelIndexedLens.view(nel) shouldEqual ((0, 1))
  }

  test("set") {
    nelIndexedLens.set(9)(nel) shouldEqual nel.copy(head = 9)
  }

  test("over") {
    nelIndexedLens.over(oneToNine)(nel) shouldEqual nel.copy(head = 9)
  }

  test("traverse") {
    val result = nelIndexedLens.traverse[Id](nel)(oneToNine)

    result shouldEqual nel.copy(head = 9)
    nelIndexedLens.overF[Id](oneToNine)(nel) shouldEqual result
  }

  test("exists") {
    nelIndexedLens.exists(_ === ((0, 1)))(nel)
  }

  test("notExists") {
    nelIndexedLens.notExists(_ === ((0, 1)))(nel) shouldEqual false
    nelIndexedLens.notExists(_ === ((1, 1)))(nel) shouldEqual true
    nelIndexedLens.notExists(_ === ((0, 2)))(nel) shouldEqual true
    nelIndexedLens.notExists(_ === ((0, 1)))(nel) shouldEqual !nelIndexedLens.exists(_ == ((0, 1)))(nel)
  }

  test("contains") {
    nelIndexedLens.contains(nel)((0, 1)) shouldEqual true
    nelIndexedLens.contains(nel)((1, 1)) shouldEqual false
  }

  test("notContains") {
    nelIndexedLens.notContains(nel)((0, 1)) shouldEqual false
    nelIndexedLens.notContains(nel)((1, 1)) shouldEqual true
    nelIndexedLens.notContains(nel)((1, 1)) shouldEqual !nelIndexedLens.contains(nel)((1, 1))
  }

  test("find") {
    nelIndexedLens.find { case (i, a) => i === 0 && a === 1 }(nel) shouldEqual 1.some
    nelIndexedLens.find(_ === ((0, 1)))(nel) shouldEqual 1.some
    nelIndexedLens.find(_._2 === 0)(nel) shouldEqual None
    nelIndexedLens.find(_ === ((1, 1)))(nel) shouldEqual None
  }

  test("use") {
    nelIndexedLens.use.runA(nel).value shouldEqual ((0, 1))
  }

  test("failover") {
    val res = nelIndexedLens.failover[Option](nel)(_._2)(strongStarTupleOfDisj, catsStdInstancesForOption)
    val negativeRes = nelIndexedLens.failover[Option](nel)(_._2)(strongStarTupleOfNegativeDisj, catsStdInstancesForOption)

    res shouldEqual Some(nel)
    negativeRes shouldEqual None
  }

  test("zipWith") {
    val secondNel = NonEmptyList.fromListUnsafe(List(8, 9, 10))
    val result = NonEmptyList.fromListUnsafe(9 :: list.tail)

    nelIndexedLens.zipWith(nel, secondNel) { case ((_, a1), (_, a2)) => a1 + a2 } shouldEqual result
  }

  test("cotraverse") {
    val cotraversedNel = nelIndexedLens.cotraverse[Id](nel)(_._2)

    cotraversedNel shouldEqual nel
    nelIndexedLens.zipWithF[Id](_._2)(nel) shouldEqual cotraversedNel
  }

  test("compose with IndexedGetter") {
    (indexedLens compose indexedGetter).view(9) shouldEqual ((0, 9))
  }

  test("compose with IndexedFold") {
    (indexedLens compose indexedFold).foldMap(9)(_._2) shouldEqual 9
  }
}
