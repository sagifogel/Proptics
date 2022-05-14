package proptics.specs

import cats.instances.string._
import cats.laws.discipline.{ExhaustiveCheck, MiniInt}
import cats.syntax.option._
import cats.{Eq, Id}
import org.scalacheck.Arbitrary._

import proptics.law.discipline._
import proptics.specs.compose._
import proptics.std.either._
import proptics.std.function._
import proptics.std.list._
import proptics.std.string._
import proptics.std.tuple._
import proptics.{Getter, IndexedGetter, Iso}

class IsoSpec extends PropticsSuite {
  val wholeIso: Iso[Whole, Int] = Iso.iso[Whole, Int](_.part)(Whole.apply)
  val combineFocus: (Whole, Whole) => Int = { case (whole1, whole2) => whole1.part + whole2.part }
  val flipped: Iso[Whole => Int => Int, Int => Whole => Int] = flip
  val curriedIso: Iso[(Whole, Whole) => Int, Whole => Whole => Int] = curried
  val uncurriedIso: Iso[Whole => Whole => Int, (Whole, Whole) => Int] = uncurried
  implicit val arrayEq: Eq[Array[String]] = Eq.instance[Array[String]]((arr1, arr2) => arr1.toList === arr2.toList)
  implicit def eqFn0(implicit ev: ExhaustiveCheck[MiniInt]): Eq[(Int, Int) => Int] = Eq.instance[(Int, Int) => Int] { (f1, f2) =>
    ev.allValues.forall { miniInt =>
      val int = miniInt.toInt

      f1(int, int) === f2(int, int)
    }
  }

  implicit def eqFn1(implicit ev: ExhaustiveCheck[MiniInt]): Eq[Int => (Int, Int)] = Eq.instance[Int => (Int, Int)] { (f1, f2) =>
    ev.allValues.forall { miniInt =>
      val int = miniInt.toInt

      f1(int) === f2(int)
    }
  }

  implicit def eqFn2(implicit ev: ExhaustiveCheck[MiniInt]): Eq[Int => Int => Int] = Eq.instance[Int => Int => Int] { (f1, f2) =>
    ev.allValues.forall { miniInt =>
      val int = miniInt.toInt

      f1(int)(int) === f2(int)(int)
    }
  }

  checkAll("Iso[Whole, Int] apply", IsoTests(wholeIso).iso)
  checkAll("Iso[Int, Int] id", IsoTests(Iso.id[Int]).iso)
  checkAll("Iso[(Int, Int) => Int, Int => Int => Int] curried", IsoTests(curried[Int, Int, Int]).iso)
  checkAll("Iso[Int => Int => Int, (Int, Int) => Int] uncurried", IsoTests(uncurried[Int, Int, Int]).iso)
  checkAll("Iso[Int => Int => Int, Int => Int => Int] flip", IsoTests(flip[Int, Int, Int]).iso)
  checkAll("Iso[(String, Int), (Int, String)] swapTuple", IsoTests(swapTuple[String, Int]).iso)
  checkAll("Iso[Either[String, Int], Either[Int, String]] swapEither", IsoTests(swapEither[String, Int]).iso)
  checkAll("Iso[String, String] involuted", IsoTests(Iso.involuted[String](identity)).iso)
  checkAll("Iso[List[Char], String] charsToString", IsoTests(charsToString).iso)
  checkAll("Iso[String, List[Char]] stringToChars", IsoTests(stringToChars).iso)
  checkAll("Iso[List[String], Array[String]] listToArray", IsoTests(listToArray[String]).iso)
  checkAll("Iso[List[String], Vector[String]] listToVector", IsoTests(listToVector[String]).iso)
  checkAll("Iso[Whole, Int] reverse twice", IsoTests(wholeIso.reverse.reverse).iso)
  checkAll("Iso[Int, Int] compose with Iso[Int, Int]", IsoTests(iso compose iso).iso)
  checkAll("Iso[Int, Int] andThen with Iso[Int, Int]", IsoTests(iso andThen iso).iso)
  checkAll("Iso[Int, Int] compose with AnIso[Int, Int]", AnIsoTests(iso compose anIso).anIso)
  checkAll("Iso[Int, Int] andThen with AnIso[Int, Int]", AnIsoTests(iso andThen anIso).anIso)
  checkAll("Iso[Int, Int] compose with Lens[Int, Int]", LensTests(iso compose lens).lens)
  checkAll("Iso[Int, Int] andThen with Lens[Int, Int]", LensTests(iso andThen lens).lens)
  checkAll("Iso[Int, Int] compose with ALens[Int, Int]", ALensTests(iso compose aLens).aLens)
  checkAll("Iso[Int, Int] andThen with ALens[Int, Int]", ALensTests(iso andThen aLens).aLens)
  checkAll("Iso[Int, Int] compose with Prism[Int, Int]", PrismTests(iso compose prism).prism)
  checkAll("Iso[Int, Int] andThen with Prism[Int, Int]", PrismTests(iso andThen prism).prism)
  checkAll("Iso[Int, Int] compose with APrism[Int, Int]", APrismTests(iso compose aPrism).aPrism)
  checkAll("Iso[Int, Int] andThen with APrism[Int, Int]", APrismTests(iso andThen aPrism).aPrism)
  checkAll("Iso[Int, Int] compose with AffineTraversal[Int, Int]", AffineTraversalTests(iso compose affineTraversal).affineTraversal)
  checkAll("Iso[Int, Int] andThen with AffineTraversal[Int, Int]", AffineTraversalTests(iso andThen affineTraversal).affineTraversal)
  checkAll("Iso[Int, Int] compose with AnAffineTraversal[Int, Int]", AnAffineTraversalTests(iso compose anAffineTraversal).anAffineTraversal)
  checkAll("Iso[Int, Int] andThen with AnAffineTraversal[Int, Int]", AnAffineTraversalTests(iso andThen anAffineTraversal).anAffineTraversal)
  checkAll("Iso[Int, Int] compose with Traversal[Int, Int]", TraversalTests(iso compose traversal).traversal)
  checkAll("Iso[Int, Int] andThen with Traversal[Int, Int]", TraversalTests(iso andThen traversal).traversal)
  checkAll("Iso[Int, Int] compose with ATraversal[Int, Int]", ATraversalTests(iso compose aTraversal).aTraversal)
  checkAll("Iso[Int, Int] andThen with ATraversal[Int, Int]", ATraversalTests(iso andThen aTraversal).aTraversal)
  checkAll("Iso[Int, Int] compose with Setter[Int, Int]", SetterTests(iso compose setter).setter)
  checkAll("Iso[Int, Int] andThen with Setter[Int, Int]", SetterTests(iso andThen setter).setter)
  checkAll("Iso[Int, Int] compose with Grate[Int, Int]", GrateTests(iso compose grate).grate)
  checkAll("Iso[Int, Int] andThen with Grate[Int, Int]", GrateTests(iso andThen grate).grate)
  checkAll("Iso[Int, Int] compose with IndexedLens[Int, Int, Int]", IndexedLensTests(iso compose indexedLens).indexedLens)
  checkAll("Iso[Int, Int] andThen with IndexedLens[Int, Int, Int]", IndexedLensTests(Iso.id[Int] andThen indexedLens).indexedLens)
  checkAll("Iso[Int, Int] compose with AnIndexedLens[Int, Int, Int]", AnIndexedLensTests(iso compose anIndexedLens).anIndexedLens)
  checkAll("Iso[Int, Int] andThen with AnIndexedLens[Int, Int, Int]", AnIndexedLensTests(Iso.id[Int] andThen anIndexedLens).anIndexedLens)
  checkAll("Iso[Int, Int] compose with IndexedTraversal[Int, Int, Int]", IndexedTraversalTests(iso compose indexedTraversal).indexedTraversal)
  checkAll("Iso[Int, Int] andThen with IndexedTraversal[Int, Int, Int]", IndexedTraversalTests(Iso.id[Int] andThen indexedTraversal).indexedTraversal)
  checkAll("Iso[Int, Int] compose with IndexedSetter[Int, Int, Int]", IndexedSetterTests(iso compose indexedSetter).indexedSetter)
  checkAll("Iso[Int, Int] andThen with IndexedSetter[Int, Int, Int]", IndexedSetterTests(Iso.id[Int] andThen indexedSetter).indexedSetter)

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
    wholeIso.traverse(whole9)(_.some) shouldEqual whole9.some
    wholeIso.traverse(whole9)(_.some) shouldEqual wholeIso.overF(_.some)(whole9)
  }

  test("exists") {
    wholeIso.exists(greaterThan5)(whole9) shouldEqual true
    wholeIso.exists(greaterThan10)(whole9) shouldEqual false
  }

  test("notExists") {
    wholeIso.notExists(greaterThan10)(whole9) shouldEqual true
    wholeIso.notExists(greaterThan5)(whole9) shouldEqual false
    wholeIso.notExists(greaterThan5)(whole9) shouldEqual !wholeIso.exists(greaterThan5)(whole9)
  }

  test("contains") {
    wholeIso.contains(9)(whole9) shouldEqual true
    wholeIso.contains(5)(whole9) shouldEqual false
  }

  test("notContains") {
    wholeIso.notContains(5)(whole9) shouldEqual true
    wholeIso.notContains(9)(whole9) shouldEqual false
    wholeIso.notContains(9)(whole9) shouldEqual !wholeIso.contains(9)(whole9)
  }

  test("find") {
    wholeIso.find(greaterThan5)(whole9) shouldEqual Some(9)
    wholeIso.find(greaterThan10)(whole9) shouldEqual None
  }

  test("use") {
    wholeIso.use.runA(whole9).value shouldEqual 9
  }

  test("zipWith") {
    wholeIso.zipWith(Whole(8), Whole(1))(_ + _) shouldEqual whole9
  }

  test("cotraverse") {
    val cotraversedWhole = wholeIso.cotraverse[Id](whole9)(identity)

    cotraversedWhole shouldEqual whole9
    wholeIso.zipWithF[Id](identity)(whole9) shouldEqual cotraversedWhole
  }

  test("curried") {
    curriedIso.view(combineFocus)(whole9)(whole9) shouldEqual 18
    curriedIso.review(combineFocus.curried)(whole9, whole9) shouldEqual 18
  }

  test("uncurried") {
    uncurriedIso.view(combineFocus.curried)(whole9, whole9) shouldEqual 18
    uncurriedIso.review(combineFocus)(whole9)(whole9) shouldEqual 18
  }

  test("flipped") {
    flipped.view(w => w.part + _)(9)(whole9) shouldEqual 18
  }

  test("compose with Getter") {
    (iso compose getter).view(9) shouldEqual 9
    (iso compose Getter[Int](_ + 1)).view(8) shouldEqual 9
  }

  test("andThen with Getter") {
    (iso andThen getter).view(9) shouldEqual 9
    (iso andThen Getter[Int](_ + 1)).view(8) shouldEqual 9
  }

  test("compose with Fold") {
    (iso compose fold).fold(9) shouldEqual 9
  }

  test("andThen with Fold") {
    (iso andThen fold).fold(9) shouldEqual 9
  }

  test("compose with review") {
    (iso compose review).review(9) shouldEqual 9
  }

  test("andThen with review") {
    (iso andThen review).review(9) shouldEqual 9
  }

  test("compose with IndexedGetter") {
    val composed = iso compose indexedGetter

    composed.foldMap(9)(_._2) shouldEqual 0
    composed.foldMap(9)(_._1) shouldEqual 9
  }

  test("andThen with IndexedGetter") {
    val composed = iso andThen indexedGetter

    composed.foldMap(9)(_._2) shouldEqual 0
    composed.foldMap(9)(_._1) shouldEqual 9
  }

  test("andThen with IndexedGetter[Int, Int, Int]") {
    val composed = Iso.id[Int] andThen IndexedGetter[Int, Int, Int]((_, 1))

    composed.view(9) shouldEqual 9
  }

  test("compose with IndexedFold") {
    val composed = iso compose indexedFold

    composed.foldMap(9)(_._2) shouldEqual 0
    composed.foldMap(9)(_._1) shouldEqual 9
  }

  test("andThen with IndexedFold") {
    val composed = iso andThen indexedFold

    composed.foldMap(9)(_._2) shouldEqual 0
    composed.foldMap(9)(_._1) shouldEqual 9
  }
}
