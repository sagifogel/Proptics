package proptics.specs

import cats.Id
import cats.data.NonEmptyList
import cats.syntax.option._

import proptics.AnIndexedLens
import proptics.internal.{Indexed, Shop}
import proptics.law.discipline._
import proptics.specs.compose._

class AnIndexedLensSpec extends PropticsSuite {
  val nelIndexedLens: AnIndexedLens[Int, NonEmptyList[Int], Int] =
    AnIndexedLens[Int, NonEmptyList[Int], Int](ls => (0, ls.head))(nel => i => nel.copy(head = i))

  checkAll("AnIndexedLens[Int, NonEmptyList[Int], Int] apply", AnIndexedLensTests(nelIndexedLens).anIndexedLens)
  checkAll("AnIndexedLens[Int, NonEmptyList[Int], Int] asLens", LensTests(nelIndexedLens.asLens).lens)
  checkAll("AnIndexedLens[Int, NonEmptyList[Int], Int] asIndexedLens", IndexedLensTests(nelIndexedLens.asIndexedLens).indexedLens)
  checkAll("AnIndexedLens[Int, Int, Int] compose with IndexedLens[Int, Int, Int]", AnIndexedLensTests(anIndexedLens compose indexedLens).anIndexedLens)
  checkAll("AnIndexedLens[Int, Int, Int] compose with AnIndexedLens[Int, Int, Int]", AnIndexedLensTests(anIndexedLens compose anIndexedLens).anIndexedLens)
  checkAll(
    "AnIndexedLens[Int, Int, Int] compose with IndexedTraversal[Int, Int, Int]",
    IndexedTraversalTests(anIndexedLens compose indexedTraversal).indexedTraversal
  )
  checkAll("AnIndexedLens[Int, Int, Int] compose with IndexedSetter[Int, Int, Int]", IndexedSetterTests(anIndexedLens compose indexedSetter).indexedSetter)

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
    nelIndexedLens.exists(_ === ((0, 1)))(nel) shouldEqual true
  }

  test("notExists") {
    nelIndexedLens.notExists(_ === ((0, 1)))(nel) shouldEqual false
    nelIndexedLens.notExists(_ === ((1, 1)))(nel) shouldEqual true
    nelIndexedLens.notExists(_ === ((0, 2)))(nel) shouldEqual true
    nelIndexedLens.notExists(_ === ((0, 1)))(nel) shouldEqual !nelIndexedLens.exists(_ == ((0, 1)))(nel)
  }

  test("contains") {
    nelIndexedLens.contains((0, 1))(nel) shouldEqual true
    nelIndexedLens.contains((1, 1))(nel) shouldEqual false
  }

  test("notContains") {
    nelIndexedLens.notContains((0, 1))(nel) shouldEqual false
    nelIndexedLens.notContains((1, 1))(nel) shouldEqual true
    nelIndexedLens.notContains((1, 1))(nel) shouldEqual !nelIndexedLens.contains((1, 1))(nel)
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

  test("withIndexedLens") {
    val one = NonEmptyList.one(head = 1)
    val shop = nelIndexedLens.withIndexedLens { get => set =>
      val shop: Shop[(Int, Int), Int, (Int, NonEmptyList[Int]), NonEmptyList[Int]] =
        Shop({ case (_, w) => get(w) }, { case (_, w) => i => set(w)(i) })

      Indexed(shop)
    }.runIndex

    shop.set((10, one))(9) shouldEqual NonEmptyList.one(head = 9)
    shop.view((10, one)) shouldEqual ((0, 1))
  }

  test("compose with IndexedGetter") {
    (anIndexedLens compose indexedGetter).view(9) shouldEqual ((0, 9))
  }

  test("compose with IndexedFold") {
    (anIndexedLens compose indexedFold).foldMap(9)(_._2) shouldEqual 9
  }
}
