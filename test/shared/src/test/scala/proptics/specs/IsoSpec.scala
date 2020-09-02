package proptics.specs

import cats.Id
import cats.instances.int._
import cats.instances.option._
import cats.syntax.option._
import org.scalacheck.Arbitrary._
import proptics.law._
import proptics.specs.Compose._
import proptics.{Iso, Iso_}

class IsoSpec extends PropticsSuite {
  val wholeIso: Iso[Whole, Int] = Iso.iso[Whole, Int](_.part)(Whole.apply)
  val combineFocus: (Whole, Whole) => Int = { case (whole1, whole2) => whole1.part + whole2.part }
  val flipped: Iso_[Whole => Int => Int, Whole => Int => Int, Int => Whole => Int, Int => Whole => Int] = Iso_.flipped
  val curried: Iso_[(Whole, Whole) => Int, (Whole, Whole) => Int, Whole => Whole => Int, Whole => Whole => Int] = Iso_.curried
  val uncurried: Iso_[Whole => Whole => Int, Whole => Whole => Int, (Whole, Whole) => Int, (Whole, Whole) => Int] = Iso_.uncurried

  checkAll("Iso[Whole, Int] apply", IsoTests(wholeIso).iso)
  checkAll("Iso[Int, Int] id", IsoTests(Iso.id[Int]).iso)
  checkAll("Iso[Whole, Int] reverse twice", IsoTests(wholeIso.reverse.reverse).iso)
  checkAll("Iso[Int, Int] with Iso[Int, Int]", IsoTests(iso compose iso).iso)
  checkAll("Iso[Int, Int] compose with AnIso[Int, Int]", AnIsoTests(iso compose anIso).anIso)
  checkAll("Iso[Int, Int] compose with Lens[Int, Int]", LensTests(iso compose lens).lens)
  checkAll("Iso[Int, Int] compose with ALens[Int, Int]", ALensTests(iso compose aLens).aLens)
  checkAll("Iso[Int, Int] compose with Prism[Int, Int]", PrismTests(iso compose prism).prism)
  checkAll("Iso[Int, Int] compose with APrism[Int, Int]", APrismTests(iso compose aPrism).aPrism)
  checkAll("Iso[Int, Int] compose with AffineTraversal[Int, Int]", AffineTraversalRules(iso compose affineTraversal))
  checkAll("Iso[Int, Int] compose with AnAffineTraversal[Int, Int]", AnAffineTraversalRules(iso compose anAffineTraversal))
  checkAll("Iso[Int, Int] compose with Traversal[Int, Int]", TraversalRules(iso compose traversal))
  checkAll("Iso[Int, Int] compose with ATraversal[Int, Int]", ATraversalRules(iso compose aTraversal))
  checkAll("Iso[Int, Int] compose with Setter[Int, Int]", SetterRules(iso compose setter))
  checkAll("Iso[Int, Int] compose with Grate[Int, Int]", GrateRules(iso compose grate))

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
    wholeIso.notExists(greaterThan5)(whole9) shouldEqual (!wholeIso.exists(greaterThan5)(whole9))
  }

  test("contains") {
    wholeIso.contains(whole9)(9) shouldEqual true
    wholeIso.contains(whole9)(5) shouldEqual false
  }

  test("notContains") {
    wholeIso.notContains(whole9)(5) shouldEqual true
    wholeIso.notContains(whole9)(9) shouldEqual false
    wholeIso.notContains(whole9)(9) shouldEqual (!wholeIso.contains(whole9)(9))
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
    curried.view(combineFocus)(whole9)(whole9) shouldEqual 18
    curried.review(combineFocus curried)(whole9, whole9) shouldEqual 18
  }

  test("uncurried") {
    uncurried.view(combineFocus curried)(whole9, whole9) shouldEqual 18
    uncurried.review(combineFocus)(whole9)(whole9) shouldEqual 18
  }

  test("flipped") {
    flipped.view(w => w.part + _)(9)(whole9) shouldEqual 18
  }

  test("compose with Getter") {
    (iso compose getter).view(9) shouldEqual 9
  }

  test("compose with Fold") {
    (iso compose fold).fold(9) shouldEqual 9
  }

  test("compose with review") {
    (iso compose review).review(9) shouldEqual 9
  }
}
