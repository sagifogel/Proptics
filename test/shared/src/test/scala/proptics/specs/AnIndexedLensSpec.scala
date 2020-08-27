package proptics.specs

import cats.Id
import cats.data.NonEmptyList
import cats.instances.int._
import cats.syntax.option._
import proptics.AnIndexedLens
import proptics.internal.{Indexed, Shop}
import proptics.law.{AnIndexedLensRules, IndexedLensRules, IndexedSetterRules, IndexedTraversalRules, LensRules}
import proptics.specs.Compose._

class AnIndexedLensSpec extends PropticsSuite {
  val nelIndexedLens: AnIndexedLens[Int, NonEmptyList[Int], Int] =
    AnIndexedLens[Int, NonEmptyList[Int], Int](ls => (0, ls.head))(nel => i => nel.copy(head = i))

  checkAll("IndexedLens apply", AnIndexedLensRules(nelIndexedLens))
  checkAll("IndexedLens asLens", LensRules(nelIndexedLens.asLens))
  checkAll("IndexedLens asIndexedLens", IndexedLensRules(nelIndexedLens.asIndexedLens))
  checkAll("compose with IndexedLens", AnIndexedLensRules(anIndexedLens compose indexedLens))
  checkAll("compose with AnIndexedLens", AnIndexedLensRules(anIndexedLens compose anIndexedLens))
  checkAll("compose with IndexedTraversal", IndexedTraversalRules(anIndexedLens compose indexedTraversal))
  checkAll("compose with IndexedSetter", IndexedSetterRules(anIndexedLens compose indexedSetter))

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

  test("withIndexedLens") {
    val one = NonEmptyList.one(head = 1)
    val shop = nelIndexedLens.withIndexedLens { get => set =>
      val shop: Shop[(Int, Int), Int, (Int, NonEmptyList[Int]), NonEmptyList[Int]] =
        Shop({ case (_, w) => get(w) }, { case (_, w) => i => set(w)(i) })

      Indexed(shop)
    }.runIndex

    shop.set((10, one))(9) shouldEqual NonEmptyList.one(head = 9)
    shop.get((10, one)) shouldEqual ((0, 1))
  }

  test("compose with IndexedGetter") {
    (anIndexedLens compose indexedGetter).view(9) shouldEqual ((0, 9))
  }

  test("compose with IndexedFold") {
    (anIndexedLens compose indexedFold).foldMap(9)(_._2) shouldEqual 9
  }
}
