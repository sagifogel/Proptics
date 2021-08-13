package proptics.specs

import scala.Function.const

import cats.Id
import cats.data.NonEmptyList
import cats.instances.option._
import cats.syntax.option._

import proptics._
import proptics.law.discipline._
import proptics.specs.compose._

class IndexedLensSpec extends PropticsSuite {
  val wholeIndexedLens: IndexedLens[Int, Whole, Int] =
    IndexedLens[Int, Whole, Int](w => (w.part, w.part))(w => p => w.copy(part = p))

  val nelIndexedLens: IndexedLens[Int, NonEmptyList[Int], Int] =
    IndexedLens[Int, NonEmptyList[Int], Int](ls => (ls.head, 0))(nel => i => nel.copy(head = i))

  checkAll("IndexedLens[Int, NonEmptyList[Int], Int] apply", IndexedLensTests(nelIndexedLens).indexedLens)
  checkAll("IndexedLens[Int, Whole, Int] asLens", LensTests(wholeIndexedLens.asLens).lens)
  checkAll("IndexedLens[Int, Int, Int] compose with Iso[Int, Int]", IndexedLensTests(indexedLens compose Iso.id[Int]).indexedLens)
  checkAll("IndexedLens[Int, Int, Int] andThen with Iso[Int, Int]", IndexedLensTests(indexedLens andThen Iso.id[Int]).indexedLens)
  checkAll("IndexedLens[Int, Int, Int] compose with AnIso[Int, Int]", IndexedLensTests(indexedLens compose AnIso.id[Int]).indexedLens)
  checkAll("IndexedLens[Int, Int, Int] andThen with AnIso[Int, Int]", IndexedLensTests(indexedLens andThen AnIso.id[Int]).indexedLens)
  checkAll("IndexedLens[Int, Int, Int] compose with Lens[Int, Int]", IndexedLensTests(indexedLens compose Lens.id[Int]).indexedLens)
  checkAll("IndexedLens[Int, Int, Int] andThen with Lens[Int, Int]", IndexedLensTests(indexedLens andThen Lens.id[Int]).indexedLens)
  checkAll("IndexedLens[Int, Int, Int] compose with ALens[Int, Int]", IndexedLensTests(indexedLens compose ALens.id[Int]).indexedLens)
  checkAll("IndexedLens[Int, Int, Int] andThen with ALens[Int, Int]", IndexedLensTests(indexedLens andThen ALens.id[Int]).indexedLens)
  checkAll("IndexedLens[Int, Int, Int] compose with Prism[Int, Int]", IndexedTraversalTests(indexedLens compose Prism.id[Int]).indexedTraversal)
  checkAll("IndexedLens[Int, Int, Int] andThen with Prism[Int, Int]", IndexedTraversalTests(indexedLens andThen Prism.id[Int]).indexedTraversal)
  checkAll("IndexedLens[Int, Int, Int] compose with APrism[Int, Int]", IndexedTraversalTests(indexedLens compose APrism.id[Int]).indexedTraversal)
  checkAll("IndexedLens[Int, Int, Int] andThen with APrism[Int, Int]", IndexedTraversalTests(indexedLens andThen APrism.id[Int]).indexedTraversal)
  checkAll("IndexedLens[Int, Int, Int] compose with AffineTraversal[Int, Int]", IndexedTraversalTests(indexedLens compose AffineTraversal.id[Int]).indexedTraversal)
  checkAll("IndexedLens[Int, Int, Int] andThen with AffineTraversal[Int, Int]", IndexedTraversalTests(indexedLens andThen AffineTraversal.id[Int]).indexedTraversal)
  checkAll(
    "IndexedLens[Int, Int, Int] compose with AnAffineTraversal[Int, Int]",
    IndexedTraversalTests(indexedLens compose AnAffineTraversal.id[Int]).indexedTraversal
  )
  checkAll(
    "IndexedLens[Int, Int, Int] andThen with AnAffineTraversal[Int, Int]",
    IndexedTraversalTests(indexedLens andThen AnAffineTraversal.id[Int]).indexedTraversal
  )
  checkAll("IndexedLens[Int, Int, Int] compose with Traversal[Int, Int]", IndexedTraversalTests(indexedLens compose Traversal.id[Int]).indexedTraversal)
  checkAll("IndexedLens[Int, Int, Int] andThen with Traversal[Int, Int]", IndexedTraversalTests(indexedLens andThen Traversal.id[Int]).indexedTraversal)
  checkAll("IndexedLens[Int, Int, Int] compose with ATraversal[Int, Int]", IndexedTraversalTests(indexedLens compose ATraversal.id[Int]).indexedTraversal)
  checkAll("IndexedLens[Int, Int, Int] andThen with ATraversal[Int, Int]", IndexedTraversalTests(indexedLens andThen ATraversal.id[Int]).indexedTraversal)
  checkAll("IndexedLens[Int, Int, Int] compose with Setter[Int, Int]", IndexedSetterTests(indexedLens compose Setter.id[Int]).indexedSetter)
  checkAll("IndexedLens[Int, Int, Int] andThen with Setter[Int, Int]", IndexedSetterTests(indexedLens andThen Setter.id[Int]).indexedSetter)
  checkAll("IndexedLens[Int, Int, Int] <<* IndexedLens[Int, Int, Int]", IndexedLensTests(indexedLens <<* indexedLens).indexedLens)
  checkAll("IndexedLens[Int, Int, Int] *>> IndexedLens[Int, Int, Int]", IndexedLensTests(indexedLens *>> indexedLens).indexedLens)
  checkAll("IndexedLens[Int, Int, Int] <<* AnIndexedLens[Int, Int, Int]", AnIndexedLensTests(indexedLens <<* anIndexedLens).anIndexedLens)
  checkAll("IndexedLens[Int, Int, Int] *>> AnIndexedLens[Int, Int, Int]", AnIndexedLensTests(indexedLens *>> anIndexedLens).anIndexedLens)
  checkAll("IndexedLens[Int, Int, Int] <<* IndexedTraversal[Int, Int, Int]", IndexedTraversalTests(indexedLens <<* indexedTraversal).indexedTraversal)
  checkAll("IndexedLens[Int, Int, Int] *>> indexedTraversal[Int, Int, Int]", IndexedTraversalTests(indexedLens *>> indexedTraversal).indexedTraversal)
  checkAll("IndexedLens[Int, Int, Int] <<* IndexedSetter[Int, Int, Int]", IndexedSetterTests(indexedLens <<* indexedSetter).indexedSetter)
  checkAll("IndexedLens[Int, Int, Int] *>> IndexedSetter[Int, Int, Int]", IndexedSetterTests(indexedLens *>> indexedSetter).indexedSetter)

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
    nelIndexedLens.exists(_ === ((1, 0)))(nel)
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

  test("failover") {
    val res = nelIndexedLens.failover[Option](nel)(_._1)(strongStarTupleOfDisj, catsStdInstancesForOption)
    val negativeRes = nelIndexedLens.failover[Option](nel)(_._1)(strongStarTupleOfNegativeDisj, catsStdInstancesForOption)

    res shouldEqual Some(nel)
    negativeRes shouldEqual None
  }

  test("zipWith") {
    val secondNel = NonEmptyList.fromListUnsafe(List(8, 9, 10))
    val result = NonEmptyList.fromListUnsafe(9 :: list.tail)

    nelIndexedLens.zipWith(nel, secondNel) { case ((a1, _), (a2, _)) => a1 + a2 } shouldEqual result
  }

  test("cotraverse") {
    val cotraversedNel = nelIndexedLens.cotraverse[Id](nel)(_._1)

    cotraversedNel shouldEqual nel
    nelIndexedLens.zipWithF[Id](_._1)(nel) shouldEqual cotraversedNel
  }

  test("reindex") {
    indexedLens.reindex(_.toString).view(9) shouldEqual ((9, "0"))
  }

  test("compose with Getter") {
    val composed = nelIndexedLens andThen Getter.id[Int]
    composed.view(NonEmptyList.fromListUnsafe(List(1, 2, 3))) shouldEqual ((1, 0))
  }

  test("andThen with Fold") {
    val composed = nelIndexedLens andThen Fold.id[Int]
    composed.foldMap(NonEmptyList.fromListUnsafe(List(0, 1, 2))) { case (_, i) => List(i) } shouldEqual List(0)
  }

  test("compose with Fold") {
    val composed = nelIndexedLens compose Fold.id[NonEmptyList[Int]]
    composed.foldMap(NonEmptyList.fromListUnsafe(List(0, 1, 2))) { case (_, i) => List(i) } shouldEqual List(0)
  }

  test("compose with IndexedGetter with right index") {
    val composed = IndexedLens[Int, Int, Int]((_, 1))(const(identity)) *>> indexedGetter

    composed.view(9) shouldEqual ((9, 0))
  }

  test("compose with IndexedGetter with left index") {
    val composed = IndexedLens[Int, Int, Int]((_, 1))(const(identity)) <<* indexedGetter

    composed.view(9) shouldEqual ((9, 1))
  }

  test("compose with IndexedFold with right index") {
    val composed = IndexedLens[Int, Int, Int]((_, 1))(const(identity)) *>> indexedFold

    composed.viewAll(9) shouldEqual List((9, 0))
  }

  test("compose with IndexedFold with left index") {
    val composed = IndexedLens[Int, Int, Int]((_, 1))(const(identity)) <<* indexedFold

    composed.viewAll(9) shouldEqual List((9, 1))
  }
}
