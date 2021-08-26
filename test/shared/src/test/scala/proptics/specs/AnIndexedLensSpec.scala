package proptics.specs

import scala.Function.const

import cats.Id
import cats.data.NonEmptyList
import cats.syntax.option._
import cats.catsInstancesForId

import proptics.internal.{Indexed, Shop}
import proptics.law.discipline._
import proptics.specs.compose._
import proptics.{ALens, APrism, ATraversal, AffineTraversal, AnAffineTraversal, AnIndexedLens, AnIso, Iso, Lens, Prism, Traversal}

class AnIndexedLensSpec extends PropticsSuite {
  val nelIndexedLens: AnIndexedLens[Int, NonEmptyList[Int], Int] =
    AnIndexedLens[Int, NonEmptyList[Int], Int](ls => (ls.head, 0))(nel => i => nel.copy(head = i))

  checkAll("AnIndexedLens[Int, NonEmptyList[Int], Int] apply", AnIndexedLensTests(nelIndexedLens).anIndexedLens)
  checkAll("AnIndexedLens[Int, NonEmptyList[Int], Int] asLens", LensTests(nelIndexedLens.asLens).lens)
  checkAll("AnIndexedLens[Int, NonEmptyList[Int], Int] asIndexedLens", IndexedLensTests(nelIndexedLens.asIndexedLens).indexedLens)
  checkAll("AnIndexedLens[Int, Int, Int] compose with Iso[Int, Int]", AnIndexedLensTests(anIndexedLens compose Iso.id[Int]).anIndexedLens)
  checkAll("AnIndexedLens[Int, Int, Int] andThen with Iso[Int, Int]", AnIndexedLensTests(anIndexedLens andThen Iso.id[Int]).anIndexedLens)
  checkAll("AnIndexedLens[Int, Int, Int] compose with AnIso[Int, Int]", AnIndexedLensTests(anIndexedLens compose AnIso.id[Int]).anIndexedLens)
  checkAll("AnIndexedLens[Int, Int, Int] andThen with AnIso[Int, Int]", AnIndexedLensTests(anIndexedLens andThen AnIso.id[Int]).anIndexedLens)
  checkAll("AnIndexedLens[Int, Int, Int] compose with Lens[Int, Int]", AnIndexedLensTests(anIndexedLens compose Lens.id[Int]).anIndexedLens)
  checkAll("AnIndexedLens[Int, Int, Int] andThen with Lens[Int, Int]", AnIndexedLensTests(anIndexedLens andThen Lens.id[Int]).anIndexedLens)
  checkAll("AnIndexedLens[Int, Int, Int] compose with ALens[Int, Int]", AnIndexedLensTests(anIndexedLens compose ALens.id[Int]).anIndexedLens)
  checkAll("AnIndexedLens[Int, Int, Int] andThen with ALens[Int, Int]", AnIndexedLensTests(anIndexedLens andThen ALens.id[Int]).anIndexedLens)
  checkAll("AnIndexedLens[Int, Int, Int] compose with Prism[Int, Int]", IndexedTraversalTests(anIndexedLens compose Prism.id[Int]).indexedTraversal)
  checkAll("AnIndexedLens[Int, Int, Int] andThen with Prism[Int, Int]", IndexedTraversalTests(anIndexedLens andThen Prism.id[Int]).indexedTraversal)
  checkAll("AnIndexedLens[Int, Int, Int] compose with APrism[Int, Int]", IndexedTraversalTests(anIndexedLens compose APrism.id[Int]).indexedTraversal)
  checkAll("AnIndexedLens[Int, Int, Int] andThen with APrism[Int, Int]", IndexedTraversalTests(anIndexedLens andThen APrism.id[Int]).indexedTraversal)
  checkAll(
    "AnIndexedLens[Int, Int, Int] compose with AffineTraversal[Int, Int]",
    IndexedTraversalTests(anIndexedLens compose AffineTraversal.id[Int]).indexedTraversal
  )
  checkAll(
    "AnIndexedLens[Int, Int, Int] andThen with AffineTraversal[Int, Int]",
    IndexedTraversalTests(anIndexedLens andThen AffineTraversal.id[Int]).indexedTraversal
  )
  checkAll(
    "AnIndexedLens[Int, Int, Int] compose with AnAffineTraversal[Int, Int]",
    IndexedTraversalTests(anIndexedLens compose AnAffineTraversal.id[Int]).indexedTraversal
  )
  checkAll(
    "AnIndexedLens[Int, Int, Int] andThen with AnAffineTraversal[Int, Int]",
    IndexedTraversalTests(anIndexedLens andThen AnAffineTraversal.id[Int]).indexedTraversal
  )
  checkAll("AnIndexedLens[Int, Int, Int] compose with Traversal[Int, Int]", IndexedTraversalTests(anIndexedLens compose Traversal.id[Int]).indexedTraversal)
  checkAll("AnIndexedLens[Int, Int, Int] andThen with Traversal[Int, Int]", IndexedTraversalTests(anIndexedLens andThen Traversal.id[Int]).indexedTraversal)
  checkAll("AnIndexedLens[Int, Int, Int] compose with ATraversal[Int, Int]", IndexedTraversalTests(anIndexedLens compose ATraversal.id[Int]).indexedTraversal)
  checkAll("AnIndexedLens[Int, Int, Int] andThen with ATraversal[Int, Int]", IndexedTraversalTests(anIndexedLens andThen ATraversal.id[Int]).indexedTraversal)
  checkAll("AnIndexedLens[Int, Int, Int] <<* IndexedLens[Int, Int, Int]", AnIndexedLensTests(anIndexedLens <<* indexedLens).anIndexedLens)
  checkAll("AnIndexedLens[Int, Int, Int] *>> IndexedLens[Int, Int, Int]", AnIndexedLensTests(anIndexedLens *>> indexedLens).anIndexedLens)
  checkAll("AnIndexedLens[Int, Int, Int] <<* with AnIndexedLens[Int, Int, Int]", AnIndexedLensTests(anIndexedLens <<* anIndexedLens).anIndexedLens)
  checkAll("AnIndexedLens[Int, Int, Int] *>> with AnIndexedLens[Int, Int, Int]", AnIndexedLensTests(anIndexedLens *>> anIndexedLens).anIndexedLens)
  checkAll("AnIndexedLens[Int, Int, Int] <<* IndexedTraversal[Int, Int, Int]", IndexedTraversalTests(anIndexedLens <<* indexedTraversal).indexedTraversal)
  checkAll("AnIndexedLens[Int, Int, Int] *>> IndexedTraversal[Int, Int, Int]", IndexedTraversalTests(anIndexedLens *>> indexedTraversal).indexedTraversal)
  checkAll("AnIndexedLens[Int, Int, Int] <<* IndexedSetter[Int, Int, Int]", IndexedSetterTests(anIndexedLens <<* indexedSetter).indexedSetter)
  checkAll("AnIndexedLens[Int, Int, Int] *>> IndexedSetter[Int, Int, Int]", IndexedSetterTests(anIndexedLens *>> indexedSetter).indexedSetter)

  test("view") {
    nelIndexedLens.view(nel) shouldEqual ((1, 0))
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
    nelIndexedLens.exists(_ === ((1, 0)))(nel) shouldEqual true
  }

  test("notExists") {
    nelIndexedLens.notExists(_ === ((1, 0)))(nel) shouldEqual false
    nelIndexedLens.notExists(_ === ((1, 1)))(nel) shouldEqual true
    nelIndexedLens.notExists(_ === ((2, 0)))(nel) shouldEqual true
    nelIndexedLens.notExists(_ === ((1, 0)))(nel) shouldEqual !nelIndexedLens.exists(_ == ((1, 0)))(nel)
  }

  test("contains") {
    nelIndexedLens.contains((1, 0))(nel) shouldEqual true
    nelIndexedLens.contains((1, 1))(nel) shouldEqual false
  }

  test("notContains") {
    nelIndexedLens.notContains((1, 0))(nel) shouldEqual false
    nelIndexedLens.notContains((1, 1))(nel) shouldEqual true
    nelIndexedLens.notContains((1, 1))(nel) shouldEqual !nelIndexedLens.contains((1, 1))(nel)
  }

  test("find") {
    nelIndexedLens.find { case (a, i) => i === 0 && a === 1 }(nel) shouldEqual (1, 0).some
    nelIndexedLens.find(_ === ((1, 0)))(nel) shouldEqual (1, 0).some
    nelIndexedLens.find(_._1 === 0)(nel) shouldEqual None
    nelIndexedLens.find(_ === ((1, 1)))(nel) shouldEqual None
  }

  test("use") {
    nelIndexedLens.use.runA(nel).value shouldEqual ((1, 0))
  }

  test("withIndexedLens") {
    val one = NonEmptyList.one(head = 1)
    val shop: Shop[(Int, Int), Int, (Int, NonEmptyList[Int]), NonEmptyList[Int]] = nelIndexedLens.withIndexedLens { get => set =>
      val shop: Shop[(Int, Int), Int, (Int, NonEmptyList[Int]), NonEmptyList[Int]] =
        Shop({ case (_, w) => get(w) }, { case (_, w) => i => set(w)(i) })

      Indexed(shop)
    }.runIndex

    shop.set((10, one))(9) shouldEqual NonEmptyList.one(head = 9)
    shop.view((10, one)) shouldEqual ((1, 0))
  }

  test("toShop") {
    val one = NonEmptyList.one(head = 1)
    val shop = nelIndexedLens.toShop

    shop.set(one)(9) shouldEqual NonEmptyList.one(head = 9)
    shop.view(one) shouldEqual ((1, 0))
  }

  test("reindex") {
    anIndexedLens.reindex(_.toString).view(9) shouldEqual ((9, "0"))
  }

  test("compose with IndexedGetter with right index") {
    val composed = AnIndexedLens[Int, Int, Int]((_, 1))(const(identity)) *>> indexedGetter

    composed.view(9) shouldEqual ((9, 0))
  }

  test("compose with IndexedGetter with left index") {
    val composed = AnIndexedLens[Int, Int, Int]((_, 1))(const(identity)) <<* indexedGetter

    composed.view(9) shouldEqual ((9, 1))
  }

  test("compose with IndexedFold with right index") {
    val composed = AnIndexedLens[Int, Int, Int]((_, 1))(const(identity)) *>> indexedFold

    composed.viewAll(9) shouldEqual List((9, 0))
  }

  test("compose with IndexedFold with left index") {
    val composed = AnIndexedLens[Int, Int, Int]((_, 1))(const(identity)) <<* indexedFold

    composed.viewAll(9) shouldEqual List((9, 1))
  }
}
