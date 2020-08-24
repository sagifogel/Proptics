package proptics.specs

import cats.Id
import cats.data.NonEmptyList
import cats.instances.int._
import cats.syntax.option._
import proptics.AnIndexedLens
import proptics.internal.{Indexed, Shop}
import proptics.law.{AnIndexedLensRules, IndexedLensRules, LensRules}

class AnIndexedLensSpec extends PropticsSuite {
  val anIndexedLens: AnIndexedLens[Int, NonEmptyList[Int], Int] =
    AnIndexedLens[Int, NonEmptyList[Int], Int](ls => (0, ls.head))(nel => i => nel.copy(head = i))

  checkAll("IndexedLens apply", AnIndexedLensRules(anIndexedLens))
  checkAll("IndexedLens asLens", LensRules(anIndexedLens.asLens))
  checkAll("IndexedLens asIndexedLens", IndexedLensRules(anIndexedLens.asIndexedLens))

  test("view") {
    anIndexedLens.view(nel) shouldEqual ((0, 1))
  }

  test("set") {
    anIndexedLens.set(9)(nel) shouldEqual nel.copy(head = 9)
  }

  test("over") {
    anIndexedLens.over(oneToNine)(nel) shouldEqual nel.copy(head = 9)
  }

  test("traverse") {
    val result = anIndexedLens.traverse[Id](nel)(oneToNine)

    result shouldEqual nel.copy(head = 9)
    anIndexedLens.overF[Id](oneToNine)(nel) shouldEqual result
  }

  test("exists") {
    anIndexedLens.exists(_ === ((0, 1)))(nel) shouldEqual true
  }

  test("notExists") {
    anIndexedLens.notExists(_ === ((0, 1)))(nel) shouldEqual false
    anIndexedLens.notExists(_ === ((1, 1)))(nel) shouldEqual true
    anIndexedLens.notExists(_ === ((0, 2)))(nel) shouldEqual true
    anIndexedLens.notExists(_ === ((0, 1)))(nel) shouldEqual !anIndexedLens.exists(_ == ((0, 1)))(nel)
  }

  test("contains") {
    anIndexedLens.contains(nel)((0, 1)) shouldEqual true
    anIndexedLens.contains(nel)((1, 1)) shouldEqual false
  }

  test("notContains") {
    anIndexedLens.notContains(nel)((0, 1)) shouldEqual false
    anIndexedLens.notContains(nel)((1, 1)) shouldEqual true
    anIndexedLens.notContains(nel)((1, 1)) shouldEqual !anIndexedLens.contains(nel)((1, 1))
  }

  test("find") {
    anIndexedLens.find { case (i, a) => i === 0 && a === 1 }(nel) shouldEqual 1.some
    anIndexedLens.find(_ === ((0, 1)))(nel) shouldEqual 1.some
    anIndexedLens.find(_._2 === 0)(nel) shouldEqual None
    anIndexedLens.find(_ === ((1, 1)))(nel) shouldEqual None
  }

  test("use") {
    anIndexedLens.use.runA(nel).value shouldEqual ((0, 1))
  }

  test("withIndexedLens") {
    val one = NonEmptyList.one(head = 1)
    val shop = anIndexedLens.withIndexedLens { get => set =>
      val shop: Shop[(Int, Int), Int, (Int, NonEmptyList[Int]), NonEmptyList[Int]] =
        Shop({ case (_, w) => get(w) }, { case (_, w) => i => set(w)(i) })

      Indexed(shop)
    }.runIndex

    shop.set((10, one))(9) shouldEqual NonEmptyList.one(head = 9)
    shop.get((10, one)) shouldEqual ((0, 1))
  }
}
