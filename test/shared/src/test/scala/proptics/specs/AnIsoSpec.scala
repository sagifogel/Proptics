package proptics.specs

import cats.Id
import cats.syntax.option._
import org.scalacheck.Arbitrary._

import proptics.AnIso
import proptics.internal.Exchange
import proptics.law.discipline._
import proptics.specs.compose._

class AnIsoSpec extends PropticsSuite {
  val wholeIso: AnIso[Whole, Int] = AnIso[Whole, Int](_.part)(Whole.apply)

  checkAll("AnIso[Whole, Int] apply ", AnIsoTests(wholeIso).anIso)
  checkAll("AnIso[Whole, Int] reverse twice", AnIsoTests(wholeIso.reverse.reverse).anIso)
  checkAll("AnIso[Whole, Int] asIso", IsoTests(wholeIso.asIso).iso)
  checkAll("AnIso[Int, Int] id", AnIsoTests(AnIso.id[Int]).anIso)
  checkAll("AnIso[Int, Int] compose with Iso[Int, Int]", AnIsoTests(anIso compose iso).anIso)
  checkAll("AnIso[Int, Int] andThen with Iso[Int, Int]", AnIsoTests(anIso andThen iso).anIso)
  checkAll("AnIso[Int, Int] compose with AnIso[Int, Int]", AnIsoTests(anIso compose anIso).anIso)
  checkAll("AnIso[Int, Int] andThen with AnIso[Int, Int]", AnIsoTests(anIso andThen anIso).anIso)
  checkAll("AnIso[Int, Int] compose with Lens[Int, Int]", LensTests(anIso compose lens).lens)
  checkAll("AnIso[Int, Int] andThen with Lens[Int, Int]", LensTests(anIso andThen lens).lens)
  checkAll("AnIso[Int, Int] compose with ALens[Int, Int] ", ALensTests(anIso compose aLens).aLens)
  checkAll("AnIso[Int, Int] andThen with Prism[Int, Int]", PrismTests(anIso andThen prism).prism)
  checkAll("AnIso[Int, Int] compose with Prism[Int, Int]", PrismTests(anIso compose prism).prism)
  checkAll("AnIso[Int, Int] andThen with APrism[Int, Int]", APrismTests(anIso andThen aPrism).aPrism)
  checkAll("AnIso[Int, Int] compose with APrism[Int, Int]", APrismTests(anIso compose aPrism).aPrism)
  checkAll("AnIso[Int, Int] andThen with AffineTraversal[Int, Int]", AffineTraversalTests(anIso andThen affineTraversal).affineTraversal)
  checkAll("AnIso[Int, Int] compose with AnAffineTraversal[Int, Int]", AnAffineTraversalTests(anIso compose anAffineTraversal).anAffineTraversal)
  checkAll("AnIso[Int, Int] andThen with AnAffineTraversal[Int, Int]", AnAffineTraversalTests(anIso andThen anAffineTraversal).anAffineTraversal)
  checkAll("AnIso[Int, Int] compose with Traversal[Int, Int]", TraversalTests(anIso compose traversal).traversal)
  checkAll("AnIso[Int, Int] andThen with ATraversal[Int, Int]", ATraversalTests(anIso andThen aTraversal).aTraversal)
  checkAll("AnIso[Int, Int] compose with Setter[Int, Int]", SetterTests(anIso compose setter).setter)
  checkAll("AnIso[Int, Int] andThen with Setter[Int, Int]", SetterTests(anIso andThen setter).setter)
  checkAll("AnIso[Int, Int] compose with Grate[Int, Int]", GrateTests(anIso compose grate).grate)
  checkAll("AnIso[Int, Int] andThen with Grate[Int, Int]", GrateTests(anIso andThen grate).grate)
  checkAll("AnIso[Int, Int] compose with IndexedLens[Int, Int, Int]", IndexedLensTests(anIso compose indexedLens).indexedLens)
  checkAll("AnIso[Int, Int] andThen with IndexedLens[Int, Int, Int]", IndexedLensTests(anIso andThen indexedLens).indexedLens)
  checkAll("AnIso[Int, Int] compose with AnIndexedLens[Int, Int, Int]", AnIndexedLensTests(anIso compose anIndexedLens).anIndexedLens)
  checkAll("AnIso[Int, Int] andThen with AnIndexedLens[Int, Int, Int]", AnIndexedLensTests(anIso andThen anIndexedLens).anIndexedLens)
  checkAll("AnIso[Int, Int] compose with IndexedTraversal[Int, Int, Int]", IndexedTraversalTests(anIso compose indexedTraversal).indexedTraversal)
  checkAll("AnIso[Int, Int] andThen with IndexedTraversal[Int, Int, Int]", IndexedTraversalTests(anIso andThen indexedTraversal).indexedTraversal)
  checkAll("AnIso[Int, Int] compose with IndexedSetter[Int, Int, Int]", IndexedSetterTests(anIso compose indexedSetter).indexedSetter)
  checkAll("AnIso[Int, Int] andThen with IndexedSetter[Int, Int, Int]", IndexedSetterTests(anIso andThen indexedSetter).indexedSetter)

  test("view") {
    wholeIso.view(whole9) shouldEqual 9
  }

  test("review") {
    wholeIso.review(9) shouldEqual whole9
  }

  test("set") {
    wholeIso.set(9)(Whole(1)) shouldEqual whole9
  }

  test("over") {
    wholeIso.over(_ + 1)(Whole(8)) shouldEqual whole9
  }
  test("traverse") {
    wholeIso.traverse(whole9)(_.some) shouldEqual Some(whole9)
    wholeIso.traverse(whole9)(_.some) shouldEqual wholeIso.overF(_.some)(whole9)
  }

  test("exists") {
    wholeIso.exists(greaterThan5)(whole9) shouldEqual true
    wholeIso.exists(greaterThan10)(whole9) shouldEqual false
  }

  test("notExists") {
    wholeIso.notExists(greaterThan10)(whole9) shouldEqual true
    wholeIso.notExists(greaterThan5)(whole9) shouldEqual false
    wholeIso.notExists(greaterThan5)(whole9) shouldEqual (!wholeIso.exists(greaterThan5)(whole9))
  }

  test("contains") {
    wholeIso.contains(9)(whole9) shouldEqual true
    wholeIso.contains(5)(whole9) shouldEqual false
  }

  test("notContains") {
    wholeIso.notContains(5)(whole9) shouldEqual true
    wholeIso.notContains(9)(whole9) shouldEqual false
    wholeIso.notContains(9)(whole9) shouldEqual (!wholeIso.contains(9)(whole9))
  }

  test("find") {
    wholeIso.find(greaterThan5)(whole9) shouldEqual Some(9)
    wholeIso.find(greaterThan10)(whole9) shouldEqual None
  }

  test("withIso") {
    val exchange: Exchange[Int, Int, Whole, Whole] = wholeIso.toExchange

    exchange.view(whole9) shouldEqual 9
    exchange.review(9) shouldEqual whole9
  }

  test("use") {
    wholeIso.use.runA(whole9).value shouldEqual 9
  }

  test("cotraverse") {
    val cotraversedWhole = wholeIso.cotraverse[Id](whole9)(identity)

    cotraversedWhole shouldEqual whole9
    wholeIso.zipWithF[Id](identity)(whole9) shouldEqual cotraversedWhole
  }

  test("under") {
    wholeIso.under(w => w.copy(part = w.part + 1))(8) shouldEqual 9
  }

  test("mapping") {
    val iso = wholeIso.mapping[Id, Option]

    iso.view(whole9) shouldEqual 9
    iso.review(9.some) shouldEqual whole9.some
  }

  test("dimapping") {
    val reversed = wholeIso.reverse
    val liftedAnIso = wholeIso.dimapping[* => *, * => *, Int, Int, Whole, Whole](reversed)

    liftedAnIso.view(_ + 1)(Whole(8)) shouldEqual whole9
    liftedAnIso.review(whole => whole.copy(whole.part + 1))(8) shouldEqual 9
  }

  test("compose with Getter") {
    (anIso compose getter).view(9) shouldEqual 9
  }

  test("andThen with Getter") {
    (anIso andThen getter).view(9) shouldEqual 9
  }

  test("compose with Fold") {
    (anIso compose fold).fold(9) shouldEqual 9
  }

  test("andThen with Fold") {
    (anIso andThen fold).fold(9) shouldEqual 9
  }

  test("compose with review") {
    (anIso compose review).review(9) shouldEqual 9
  }

  test("andThen with review") {
    (anIso andThen review).review(9) shouldEqual 9
  }

  test("compose with IndexedGetter") {
    val composed = anIso compose indexedGetter

    composed.foldMap(9)(_._2) shouldEqual 0
    composed.foldMap(9)(_._1) shouldEqual 9
  }

  test("andThen with IndexedGetter") {
    val composed = anIso andThen indexedGetter

    composed.foldMap(9)(_._2) shouldEqual 0
    composed.foldMap(9)(_._1) shouldEqual 9
  }

  test("compose with IndexedFold") {
    val composed = anIso compose indexedFold

    composed.foldMap(9)(_._2) shouldEqual 0
    composed.foldMap(9)(_._1) shouldEqual 9
  }

  test("andThen with IndexedFold") {
    val composed = anIso andThen indexedFold

    composed.foldMap(9)(_._2) shouldEqual 0
    composed.foldMap(9)(_._1) shouldEqual 9
  }
}
